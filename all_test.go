// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"go/build"
	"go/scanner"
	"go/token"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"testing"

	"github.com/cznic/lex"
	dfa "github.com/cznic/lexer"
	"github.com/cznic/y"
	"github.com/edsrzf/mmap-go"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "// caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "// \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func dbg(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "// dbg %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func TODO(...interface{}) string { //TODOOK
	_, fn, fl, _ := runtime.Caller(1)
	return fmt.Sprintf("// TODO: %s:%d:\n", path.Base(fn), fl) //TODOOK
}

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO, (*parser).todo) //TODOOK
}

// ============================================================================

const (
	lfile  = "testdata/scanner/scanner.l"
	yfile  = "testdata/parser/y/parser.y"
	ycover = "testdata/parser/ycover.go"
)

type yParser struct {
	*y.Parser
	terminals []*y.Symbol
	tok2sym   map[token.Token]*y.Symbol
}

var (
	oN = flag.Int("N", -1, "")
	_  = flag.String("out", "", "where to put y.output")
	_  = flag.Bool("closures", false, "closures")

	lexL = func() *lex.L {
		lf, err := os.Open(lfile)
		if err != nil {
			panic(err)
		}

		l, err := lex.NewL(lfile, bufio.NewReader(lf), false, false)
		if err != nil {
			panic(err)
		}

		return l
	}()

	yp0 = func() *yParser {
		var closures bool
		var fn string
		for i, v := range os.Args {
			if i == 0 {
				continue
			}

			switch v {
			case "-closures":
				closures = true
			case "-out":
				fn = os.Args[i+1]
			}
		}
		fs := token.NewFileSet()
		var out bytes.Buffer
		p, err := y.ProcessFile(fs, yfile, &y.Options{
			Closures:  closures,
			Reducible: true,
			Report:    &out,
		})
		if fn != "" {
			if err := ioutil.WriteFile(fn, out.Bytes(), 0644); err != nil {
				panic(err)
			}
		}

		if err != nil {
			panic(err)
		}

		m := make(map[token.Token]*y.Symbol, len(p.Syms))
		for k, v := range p.Syms {
			if !v.IsTerminal || k == "BODY" || k[0] == '_' {
				continue
			}

			switch {
			case k[0] >= 'A' && k[0] <= 'Z':
				if tok, ok := str2token[k]; ok {
					m[tok] = v
					break
				}

				l := v.LiteralString
				if l == "" {
					panic(fmt.Errorf("no token for %q", k))
				}

				if tok, ok := str2token[l[1:len(l)-1]]; ok {
					m[tok] = v
					break
				}

				panic(k)
			case k[0] == '\'':
				tok := str2token[k[1:2]]
				m[tok] = v
			default:
			}
		}
		m[token.EOF] = p.Syms["$end"]
		m[tokenBODY] = p.Syms["BODY"]
		var t []*y.Symbol
		for _, v := range p.Syms {
			if v.IsTerminal {
				t = append(t, v)
			}
		}
		return &yParser{
			Parser:    p,
			terminals: t,
			tok2sym:   m,
		}
	}()

	str2token = func() map[string]token.Token {
		m := map[string]token.Token{}
		for i := token.IDENT; i <= maxTokenToken; i++ {
			s := strings.ToUpper(i.String())
			if _, ok := m[s]; ok {
				panic(fmt.Errorf("internal error %q", s))
			}

			m[s] = i
		}
		m["ILLEGAL"] = token.ILLEGAL
		return m
	}()

	gorootTestFiles = func() (r []string) {
		if err := filepath.Walk(filepath.Join(runtime.GOROOT(), "src"), func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}

			if !info.IsDir() {
				return nil
			}

			if base := filepath.Base(path); strings.HasPrefix(base, ".") ||
				strings.HasPrefix(base, "_") ||
				base == "testdata" {
				return filepath.SkipDir
			}

			p, err := build.ImportDir(path, 0)
			if err != nil {
				if _, ok := err.(*build.NoGoError); ok {
					return nil
				}

				return err
			}

			for _, v := range p.GoFiles {
				r = append(r, filepath.Join(p.Dir, v))
			}
			for _, v := range p.TestGoFiles {
				r = append(r, filepath.Join(p.Dir, v))
			}
			return nil
		}); err != nil {
			panic(err)
		}

		return r[:len(r):len(r)]
	}()

	stdTestFiles = func() (r []string) {
		cmd := filepath.Join(runtime.GOROOT(), "src", "cmd")
		for _, v := range gorootTestFiles {
			if !strings.Contains(v, cmd) && !strings.HasSuffix(v, "_test.go") {
				r = append(r, v)
			}
		}
		return r[:len(r):len(r)]
	}()
)

func errString(err error) string {
	var b bytes.Buffer
	scanner.PrintError(&b, err)
	return strings.TrimSpace(b.String())
}

func testScannerStates(t *testing.T) {
	mn := len(lexL.Dfa)
	mn0 := mn
	m := make([]bool, mn+1) // 1-based state.Index.
	fs := token.NewFileSet()
	fs2 := token.NewFileSet()
	var ss scanner.Scanner
	l := newLexer(nil, nil)
	nerr := 0

	var cases, sum int
	var f func(string, *dfa.NfaState)
	f = func(prefix string, s *dfa.NfaState) {
		if nerr >= 10 {
			return
		}

		if m[s.Index] {
			return
		}

		m[s.Index] = true

		if len(s.NonConsuming) != 0 {
			panic("internal error")
		}

		next := make([]*dfa.NfaState, classNext)
		for _, e := range s.Consuming {
			switch x := e.(type) {
			case *dfa.RangesEdge:
				if x.Invert {
					panic("internal error")
				}

				for _, v := range x.Ranges.R16 {
					for c := v.Lo; c <= v.Hi; c += v.Stride {
						if c >= classNext {
							continue
						}

						if next[c] != nil {
							panic("internal error")
						}

						next[c] = x.Targ
					}
				}
				for _, v := range x.Ranges.R32 {
					for c := v.Lo; c <= v.Hi; c += v.Stride {
						if c >= classNext {
							continue
						}

						if next[c] != nil {
							panic("internal error")
						}

						next[c] = x.Targ
					}
				}
			case *dfa.RuneEdge:
				c := x.Rune
				if next[c] != nil {
					panic("internal error")
				}

				next[c] = x.Targ
			default:
				panic(fmt.Errorf("internal error: %T", x))
			}
		}
		for c, nx := range next {
			iCase := cases
			cases++
			src := prefix
			switch c {
			case classEOF:
				// nop
			case classNonASCII:
				src += "á"
			default:
				src += string(c)
			}

			tf := fs.AddFile("", -1, len(src))
			tf2 := fs2.AddFile("", -1, len(src))
			errCnt := 0
			b := []byte(src)
			var errs, errs2 scanner.ErrorList
			l.init(tf, b)
			l.errHandler = func(pos token.Pos, msg string, args ...interface{}) {
				errCnt++
				errs.Add(tf.Position(pos), fmt.Sprintf(msg, args...))
			}
			ss.Init(tf2, b, func(pos token.Position, msg string) {
				errs2.Add(pos, msg)
			}, 0)
			sum += len(src)
			for i := 0; nerr <= 10; i++ {
				errs = nil
				errs2 = nil
				l.errorCount = 0
				ss.ErrorCount = 0
				ofs, tok := l.scan()
				pos := tf.Pos(ofs)
				lit := string(l.lit)
				pos2, tok2, lit2 := ss.Scan()
				if n := *oN; n < 0 || n == iCase {
					if g, e := l.errorCount != 0, ss.ErrorCount != 0; g != e {
						nerr++
						t.Errorf(
							"%6d: errors[%d] %q(|% x|), %v %v\nall got\n%s\nall exp\n%s",
							iCase, i, src, src, l.errorCount, ss.ErrorCount, errString(errs), errString(errs2),
						)
					}
					if g, e := pos, pos2; g != e {
						nerr++
						t.Errorf(
							"%6d: pos[%d] %q(|% x|), %s(%v) %s(%v)",
							iCase, i, src, src, tf.Position(g), g, tf2.Position(e), e,
						)
					}
					if g, e := tok, tok2; g != e {
						nerr++
						t.Errorf("%6d: tok[%d] %q(|% x|) %s %s", iCase, i, src, src, g, e)
					}
					if l.errorCount+ss.ErrorCount != 0 || tok == token.ILLEGAL {
						continue
					}

					if lit2 == "" && tok2 != token.EOF {
						lit2 = tok2.String()
					}
					if g, e := lit, lit2; g != e {
						nerr++
						t.Errorf("%6d: lit[%d] %q(|% x|), %q(|% x|) %q(|% x|)", iCase, i, src, src, g, g, e, e)
					}
					if nerr >= 10 {
						return
					}
				}
				if tok == token.EOF || tok2 == token.EOF {
					break
				}
			}
			if c == classEOF || nx == nil {
				continue
			}

			f(src, nx)
		}
		mn--
	}
	f("", lexL.Dfa[0])
	if mn != 0 {
		t.Errorf("states covered: %d/%d", mn0-mn, mn0)
	} else {
		t.Logf("states covered: %d/%d", mn0-mn, mn0)
	}
	t.Logf("test cases %v, total src len %v", cases, sum)
}

func testScannerBugs(t *testing.T) {
	type toks []struct {
		ofs int
		pos string
		tok token.Token
		lit string
	}
	fs := token.NewFileSet()
	l := newLexer(nil, nil)
	n := *oN
	nerr := 0
	for i, v := range []struct {
		src  string
		toks toks
	}{
		{" (", toks{{1, "1:2", token.LPAREN, "("}}},
		{" (\n", toks{{1, "1:2", token.LPAREN, "("}}},
		{" z ", toks{{1, "1:2", token.IDENT, "z"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{" z", toks{{1, "1:2", token.IDENT, "z"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{" za ", toks{{1, "1:2", token.IDENT, "za"}, {4, "1:5", token.SEMICOLON, "\n"}}},
		{" za", toks{{1, "1:2", token.IDENT, "za"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{" « ", toks{{1, "1:2", tokenLTLT, "«"}}},
		{" » ", toks{{1, "1:2", tokenGTGT, "»"}}},
		{"", nil},
		{"'\\U00000000'!", toks{{0, "1:1", token.CHAR, "'\\U00000000'"}, {12, "1:13", token.NOT, "!"}}},
		{"'\\U00000000'", toks{{0, "1:1", token.CHAR, "'\\U00000000'"}, {12, "1:13", token.SEMICOLON, "\n"}}},
		{"'\\u0000'!", toks{{0, "1:1", token.CHAR, "'\\u0000'"}, {8, "1:9", token.NOT, "!"}}},
		{"'\\u0000'", toks{{0, "1:1", token.CHAR, "'\\u0000'"}, {8, "1:9", token.SEMICOLON, "\n"}}},
		{"'\\x00'!", toks{{0, "1:1", token.CHAR, "'\\x00'"}, {6, "1:7", token.NOT, "!"}}},
		{"'\\x00'", toks{{0, "1:1", token.CHAR, "'\\x00'"}, {6, "1:7", token.SEMICOLON, "\n"}}},
		{"( ", toks{{0, "1:1", token.LPAREN, "("}}},
		{"(", toks{{0, "1:1", token.LPAREN, "("}}},
		{"/***/func", toks{{5, "1:6", token.FUNC, "func"}}},
		{"/**/func", toks{{4, "1:5", token.FUNC, "func"}}},
		{"/*\n */\nfunc ", toks{{7, "3:1", token.FUNC, "func"}}},
		{"/*\n *\n */\nfunc ", toks{{10, "4:1", token.FUNC, "func"}}},
		{"/*\n*/\nfunc ", toks{{6, "3:1", token.FUNC, "func"}}},
		{"/*\n\n*/\nfunc ", toks{{7, "4:1", token.FUNC, "func"}}},
		{"//", nil},
		{"//\n", nil},
		{"//\n//", nil},
		{"//\n//\n", nil},
		{"//\n//\n@", toks{{6, "3:1", token.ILLEGAL, "@"}}},
		{"//\n//\nz", toks{{6, "3:1", token.IDENT, "z"}, {7, "3:2", token.SEMICOLON, "\n"}}},
		{"//\n//\nz1", toks{{6, "3:1", token.IDENT, "z1"}, {8, "3:3", token.SEMICOLON, "\n"}}},
		{"//\n@", toks{{3, "2:1", token.ILLEGAL, "@"}}},
		{"//\nz", toks{{3, "2:1", token.IDENT, "z"}, {4, "2:2", token.SEMICOLON, "\n"}}},
		{"//\nz1", toks{{3, "2:1", token.IDENT, "z1"}, {5, "2:3", token.SEMICOLON, "\n"}}},
		{";\xf0;", toks{{0, "1:1", token.SEMICOLON, ";"}, {1, "1:2", token.IDENT, "\xf0"}, {2, "1:3", token.SEMICOLON, ";"}}},
		{"\"\\U00000000\"!", toks{{0, "1:1", token.STRING, "\"\\U00000000\""}, {12, "1:13", token.NOT, "!"}}},
		{"\"\\U00000000\"", toks{{0, "1:1", token.STRING, "\"\\U00000000\""}, {12, "1:13", token.SEMICOLON, "\n"}}},
		{"\"\\u0000\"!", toks{{0, "1:1", token.STRING, "\"\\u0000\""}, {8, "1:9", token.NOT, "!"}}},
		{"\"\\u0000\"", toks{{0, "1:1", token.STRING, "\"\\u0000\""}, {8, "1:9", token.SEMICOLON, "\n"}}},
		{"\"\\x00\"!", toks{{0, "1:1", token.STRING, "\"\\x00\""}, {6, "1:7", token.NOT, "!"}}},
		{"\"\\x00\"", toks{{0, "1:1", token.STRING, "\"\\x00\""}, {6, "1:7", token.SEMICOLON, "\n"}}},
		{"\xf0", toks{{0, "1:1", token.IDENT, "\xf0"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"\xf0;", toks{{0, "1:1", token.IDENT, "\xf0"}, {1, "1:2", token.SEMICOLON, ";"}}},
		{"a/**/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a/**//**/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a/*\n*/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a/*\n*//**/", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a//", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"a//\n", toks{{0, "1:1", token.IDENT, "a"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"d/*\\\n*/0", toks{{0, "1:1", token.IDENT, "d"}, {1, "1:2", token.SEMICOLON, "\n"}, {7, "2:3", token.INT, "0"}, {8, "2:4", token.SEMICOLON, "\n"}}},
		{"import ( ", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"import (", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"import (\n", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"import (\n\t", toks{{0, "1:1", token.IMPORT, "import"}, {7, "1:8", token.LPAREN, "("}}},
		{"z ", toks{{0, "1:1", token.IDENT, "z"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{"z w", toks{{0, "1:1", token.IDENT, "z"}, {2, "1:3", token.IDENT, "w"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{"z", toks{{0, "1:1", token.IDENT, "z"}, {1, "1:2", token.SEMICOLON, "\n"}}},
		{"za ", toks{{0, "1:1", token.IDENT, "za"}, {3, "1:4", token.SEMICOLON, "\n"}}},
		{"za wa", toks{{0, "1:1", token.IDENT, "za"}, {3, "1:4", token.IDENT, "wa"}, {5, "1:6", token.SEMICOLON, "\n"}}},
		{"za", toks{{0, "1:1", token.IDENT, "za"}, {2, "1:3", token.SEMICOLON, "\n"}}},
		{"«", toks{{0, "1:1", tokenLTLT, "«"}}},
		{"»", toks{{0, "1:1", tokenGTGT, "»"}}},
	} {
		if n >= 0 && i != n {
			continue
		}

		src := v.src
		l.init(fs.AddFile("", -1, len(src)), []byte(src))
		for j, v := range v.toks {
			ofs, tok := l.scan()
			if g, e := ofs, v.ofs; g != e {
				nerr++
				t.Errorf("%v ofs[%d] %q(|% x|) %v %v", i, j, src, src, g, e)
			}
			if g, e := l.f.Position(l.f.Pos(ofs)).String(), v.pos; g != e {
				nerr++
				t.Errorf("%v pos[%d] %q(|% x|) %q %q", i, j, src, src, g, e)
			}
			if g, e := tok, v.tok; g != e {
				nerr++
				t.Errorf("%v tok[%d] %q(|% x|) %q %q", i, j, src, src, g, e)
			}
			if g, e := string(l.lit), v.lit; g != e {
				nerr++
				t.Errorf("%v lit[%d] %q(|% x|) %q(|% x|) %q(|% x|)", i, j, src, src, g, g, e, e)
			}
			if nerr >= 10 {
				return
			}
		}
		ofs, tok := l.scan()
		if g, e := tok, token.EOF; g != e {
			nerr++
			t.Errorf("%v tok %q(|% x|) %q %q", i, src, src, g, e)
		}
		if g, e := ofs, len(src); g != e {
			nerr++
			t.Errorf("%v ofs %q(|% x|) %v %v", i, src, src, g, e)
		}
		if nerr >= 10 {
			return
		}
	}
}

func testScanner(t *testing.T, paths []string) {
	fs := token.NewFileSet()
	var s scanner.Scanner
	l := newLexer(nil, nil)
	sum := 0
	toks := 0
	files := 0
	for _, path := range paths {
		src, err := ioutil.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		sum += len(src)
		f := fs.AddFile(path, -1, len(src))
		f2 := fs.AddFile(path, -1, len(src))
		var se scanner.ErrorList
		l.init(f, src)
		l.errHandler = func(pos token.Pos, msg string, arg ...interface{}) {
			se.Add(l.f.Position(pos), fmt.Sprintf(msg, arg...))
		}
		s.Init(f2, src, nil, 0)
		files++
		for {
			ofs, gt := l.scan()
			toks++
			glit := string(l.lit)
			pos, et, lit := s.Scan()
			position := f2.Position(pos)
			if g, e := l.f.Position(l.f.Pos(ofs)), position; g != e {
				t.Fatalf("%s: position mismatch, expected %s", g, e)
			}

			if l.errorCount != 0 {
				t.Fatal(se)
			}

			if gt == token.EOF {
				if et != token.EOF {
					t.Fatalf("%s: unexpected eof", position)
				}

				break
			}

			if g, e := gt, et; g != e {
				t.Fatalf("%s: token mismatch %q %q", position, g, e)
			}

			if lit == "" {
				lit = gt.String()
			}
			if g, e := glit, lit; g != e {
				t.Fatalf("%s: literal mismatch %q %q", position, g, e)
			}
		}
	}
	t.Logf("files: %v, toks: %v, bytes %v", files, toks, sum)
}

func TestScanner(t *testing.T) {
	_ = t.Run("States", testScannerStates) &&
		t.Run("Bugs", testScannerBugs) &&
		t.Run("GOROOT", func(*testing.T) { testScanner(t, gorootTestFiles) })
}

func BenchmarkScanner(b *testing.B) {
	fs := token.NewFileSet()
	l := newLexer(nil, nil)

	b.Run("File", func(b *testing.B) {
		src, err := ioutil.ReadFile(filepath.Join(runtime.GOROOT(), "src", "go", "scanner", "scanner.go"))
		if err != nil {
			b.Fatal(err)
		}

		f := fs.AddFile("scanner.go", -1, len(src))
		b.SetBytes(int64(len(src)))
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			l.init(f, src)
			for {
				_, tok := l.scan()
				if tok == token.EOF {
					break
				}
			}
		}
	})

	b.Run("Std", func(b *testing.B) {
		b.ResetTimer()
		var sum int
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdTestFiles {
				src, err := ioutil.ReadFile(v)
				if err != nil {
					b.Fatal(err)
				}

				sum += len(src)
				f := fs.AddFile(v, -1, len(src))
				l.init(f, src)
				for {
					_, tok := l.scan()
					if tok == token.EOF {
						break
					}
				}
			}
		}
		b.SetBytes(int64(sum))
	})

	b.Run("StdMMap", func(b *testing.B) {
		b.ResetTimer()
		var sum int
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdTestFiles {
				f0, err := os.Open(v)
				if err != nil {
					b.Fatal(err)
				}

				src, err := mmap.Map(f0, 0, 0)
				if err != nil {
					b.Fatal(err)
				}

				sum += len(src)
				f := fs.AddFile(v, -1, len(src))
				l.init(f, src)
				for {
					_, tok := l.scan()
					if tok == token.EOF {
						break
					}
				}
				src.Unmap()
				f0.Close()
			}
		}
		b.SetBytes(int64(sum))
	})

	b.Run("StdParalel", func(b *testing.B) {
		c := make(chan error, len(stdTestFiles))
		b.ResetTimer()
		var sum int
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdTestFiles {
				go func(v string) {
					src, err := ioutil.ReadFile(v)
					if err != nil {
						c <- err
					}

					sum += len(src)
					f := fs.AddFile(v, -1, len(src))
					l := newLexer(f, src)
					for {
						_, tok := l.scan()
						if tok == token.EOF {
							break
						}
					}
					c <- nil
				}(v)
			}
			for range stdTestFiles {
				if err := <-c; err != nil {
					b.Fatal(err)
				}
			}
		}
		b.SetBytes(int64(sum))
	})

	b.Run("StdParalelMMap", func(b *testing.B) {
		c := make(chan error, len(stdTestFiles))
		b.ResetTimer()
		var sum int
		for i := 0; i < b.N; i++ {
			sum = 0
			for _, v := range stdTestFiles {
				go func(v string) {
					f0, err := os.Open(v)
					if err != nil {
						c <- err
					}

					defer f0.Close()

					src, err := mmap.Map(f0, 0, 0)
					if err != nil {
						c <- err
					}

					defer src.Unmap()

					sum += len(src)
					f := fs.AddFile(v, -1, len(src))
					l := newLexer(f, src)
					for {
						_, tok := l.scan()
						if tok == token.EOF {
							break
						}
					}
					c <- nil
				}(v)
			}
			for range stdTestFiles {
				if err := <-c; err != nil {
					b.Fatal(err)
				}
			}
		}
		b.SetBytes(int64(sum))
	})
}

type ylex struct {
	*lexer
	lbrace        int
	lbraceRule    int
	lbraceStack   []int
	loophack      bool
	loophackStack []bool
	ofs           int
	p             *yparser
	tok           token.Token
}

func (l *ylex) init(f *token.File, src []byte) {
	l.lexer.init(f, src)
	l.lbrace = 0
	l.lbraceStack = l.lbraceStack[:0]
	l.loophack = false
	l.loophackStack = l.loophackStack[:0]
}

func newYlex(l *lexer, p *yparser) *ylex {
	yl := &ylex{lexer: l, p: p}
	for _, v := range p.Rules {
		if v.Sym.Name == "lbrace" {
			yl.lbraceRule = v.RuleNum
			break
		}
	}
	return yl
}

func (l *ylex) lex() (int, *y.Symbol) {
	ofs, tok := l.scan()
	sym, ok := l.p.tok2sym[tok]
	if !ok {
		panic(fmt.Sprintf("%s: missing symbol for token %q", l.position(ofs), tok))
	}

	switch tok {
	case token.FOR, token.IF, token.SELECT, token.SWITCH:
		l.loophack = true
	case token.LPAREN, token.LBRACK:
		if l.loophack || len(l.loophackStack) != 0 {
			l.loophackStack = append(l.loophackStack, l.loophack)
			l.loophack = false
		}
	case token.RPAREN, token.RBRACK:
		if n := len(l.loophackStack); n != 0 {
			l.loophack = l.loophackStack[n-1]
			l.loophackStack = l.loophackStack[:n-1]
		}
	case token.LBRACE:
		l.lbrace++
		if l.loophack {
			tok = tokenBODY
			sym = l.p.tok2sym[tok]
			l.loophack = false
		}
	case token.RBRACE:
		l.lbrace--
		if n := len(l.lbraceStack); n != 0 && l.lbraceStack[n-1] == l.lbrace {
			l.lbraceStack = l.lbraceStack[:n-1]
			l.loophack = true
		}
	}
	l.tok = tok
	l.ofs = ofs
	return ofs, sym
}

func (l *ylex) fixLbr() {
	n := l.lbrace - 1
	switch l.tok {
	case token.RBRACE:
		l.loophack = true
		return
	case token.LBRACE:
		n--
	}

	l.lbraceStack = append(l.lbraceStack, n)
}

type yparser struct {
	*yParser
	reduce func(int)
	trace  func(int)
	yyS    []int
	yySyms []*y.Symbol
	yychar *y.Symbol
}

func newYParser(reduce, trace func(int)) *yparser {
	return &yparser{
		yParser: yp0,
		reduce:  reduce,
		trace:   trace,
	}
}

func (p *yparser) parse(lex func(int) *y.Symbol) error {
	yystate := 0
	p.yyS = p.yyS[:0]
	p.yySyms = p.yySyms[:0]
	p.yychar = nil
	for {
		p.yyS = append(p.yyS, yystate)
		if p.trace != nil {
			p.trace(yystate)
		}
		if p.yychar == nil {
			p.yychar = lex(yystate)
		}
		switch typ, arg := p.action(yystate, p.yychar).Kind(); typ {
		case 'a':
			return nil
		case 's':
			p.yySyms = append(p.yySyms, p.yychar)
			p.yychar = nil
			yystate = arg
		case 'r':
			rule := p.Rules[arg]
			if p.reduce != nil {
				p.reduce(rule.RuleNum)
			}
			n := len(p.yyS)
			m := len(rule.Components)
			p.yyS = p.yyS[:n-m]
			p.yySyms = append(p.yySyms[:n-m-1], rule.Sym)
			n -= m
			_, yystate = p.action(p.yyS[n-1], rule.Sym).Kind()
		default:
			return p.fail(yystate)
		}
	}
}

func (p *yparser) fail(yystate int) error {
	var a []string
	for _, v := range p.Table[yystate] {
		nm := v.Sym.Name
		if nm == "$end" {
			nm = "EOF"
		}
		if l := v.Sym.LiteralString; l != "" {
			nm = l
		}
		a = append(a, nm)
	}
	sort.Strings(a)
	return fmt.Errorf("no action for %s in state %d, follow set: [%v]", p.yychar, yystate, strings.Join(a, ", "))
}

func (p *yparser) report(states []int) string {
	var b bytes.Buffer
	for _, state := range states {
		fmt.Fprintf(&b, "state %d //", state)
		syms, la := p.States[state].Syms0()
		for _, v := range syms {
			fmt.Fprintf(&b, " %s", v.Name)
		}
		if la != nil {
			fmt.Fprintf(&b, " [%s]", la.Name)
		}
		w := 0
		for _, v := range p.Table[state] {
			nm := v.Sym.Name
			if len(nm) > w {
				w = len(nm)
			}
		}
		w = -w - 2
		a := []string{"\n"}
		g := false
		for _, v := range p.Table[state] {
			nm := v.Sym.Name
			switch typ, arg := v.Kind(); typ {
			case 'a':
				a = append(a, fmt.Sprintf("    %*saccept", w, nm))
			case 's':
				a = append(a, fmt.Sprintf("    %*sshift and goto state %v", w, nm, arg))
			case 'r':
				a = append(a, fmt.Sprintf("    %*sreduce using rule %v (%s)", w, nm, arg, p.Rules[arg].Sym.Name))
			case 'g':
				if !g {
					a = append(a, "")
					g = true
				}
				a = append(a, fmt.Sprintf("    %*sgoto state %v", w, nm, arg))
			default:
				panic("internal error")
			}
		}
		b.WriteString(strings.Join(a, "\n"))
		b.WriteString("\n----\n")
	}
	return b.String()
}

func (*yparser) tok2str(tok token.Token) string {
	switch tok {
	case token.ILLEGAL:
		return "@"
	case token.COMMENT, token.EOF, tokenNL, tokenLTLT, tokenGTGT:
		return ""
	case token.IDENT:
		return "a"
	case token.INT:
		return "1"
	case token.FLOAT:
		return "2.3"
	case token.IMAG:
		return "4i"
	case token.CHAR:
		return "'b'"
	case token.STRING:
		return `"c"`
	case tokenBODY:
		return "{"
	default:
		return tok.String()
	}
}

func (p *yparser) newCover() map[int]struct{} {
	r := make(map[int]struct{}, len(p.States))
	for i := range p.States {
		r[i] = struct{}{}
	}
	return r
}

func (p *yparser) followList(state int) (r []*y.Symbol) {
	for _, v := range p.Table[state] {
		if v.Sym.IsTerminal {
			r = append(r, v.Sym)
		}
	}
	return r
}

func (p *yparser) followSet(state int) map[*y.Symbol]struct{} {
	l := p.followList(state)
	m := make(map[*y.Symbol]struct{}, len(l))
	for _, v := range l {
		m[v] = struct{}{}
	}
	return m
}

func (p *yparser) action(state int, sym *y.Symbol) *y.Action {
	for _, v := range p.Table[state] {
		if v.Sym == sym {
			return &v
		}
	}
	return nil
}

func testParserYacc(t *testing.T, files []string) {
	var cover map[int]struct{}
	var yl *ylex
	yp := newYParser(
		func(rule int) {
			if rule == yl.lbraceRule {
				yl.fixLbr()
			}
		},
		func(state int) { delete(cover, state) },
	)
	cover = yp.newCover()
	cn0 := len(cover)
	fs := token.NewFileSet()
	l := newLexer(nil, nil)
	yl = newYlex(l, yp)
	sum := 0
	toks := 0
	nfiles := 0
	for _, path := range files {
		src, err := ioutil.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		f := fs.AddFile(path, -1, len(src))
		nfiles++
		sum += len(src)
		yl.init(f, src)
		var ofs int
		if err = yp.parse(
			func(int) (s *y.Symbol) {
				ofs, s = yl.lex()
				toks++
				return s
			},
		); err != nil {
			t.Fatalf("%s: %v", yl.position(ofs), err)
		}
	}
	if cn := len(cover); cn != 0 {
		t.Errorf("states covered: %d/%d", cn0-cn, cn0)
	} else {
		t.Logf("states covered: %d/%d", cn0-cn, cn0)
	}
	t.Logf("files: %v, toks: %v, bytes %v", nfiles, toks, sum)
	e := -1
	for s := range cover {
		if e < 0 || e > s {
			e = s
		}
	}
	if e >= 0 {
		t.Errorf("states %v, unused %v, first unused state %v", len(yp.States), len(cover), e)
	}
}

func testParser(t *testing.T, files []string) {
	fs := token.NewFileSet()
	l := newLexer(nil, nil)
	for _, path := range files {
		src, err := ioutil.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		f := fs.AddFile(path, -1, len(src))
		l.init(f, src)
		p := newParser(l)
		//p.parse()
		if !p.ok {
			t.Fatal("TODO")
		}
	}
}

func testParserStates(t *testing.T) {
	//TODO check correct accepted/rejected tokens in all states.
}

func TestParser(t *testing.T) {
	cover := append(gorootTestFiles, ycover)
	_ = t.Run("Yacc", func(t *testing.T) { testParserYacc(t, cover) }) &&
		t.Run("GOROOT", func(t *testing.T) { testParser(t, cover) }) &&
		t.Run("States", testParserStates)

}
