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
	"runtime/debug"
	"sort"
	"strings"
	"testing"

	"github.com/cznic/lex"
	dfa "github.com/cznic/lexer"
	"github.com/cznic/mathutil"
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

func stack() []byte { return debug.Stack() }

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO, (*parser).todo, stack) //TODOOK
}

// ============================================================================

const (
	lexFile   = "testdata/scanner/scanner.l"
	yaccCover = "testdata/parser/ycover.go"
	yaccFile  = "testdata/parser/parser.y"
)

type yParser struct {
	*y.Parser
	reports   [][]byte
	terminals []*y.Symbol
	tok2sym   map[token.Token]*y.Symbol
}

var (
	oN = flag.Int("N", -1, "")
	_  = flag.String("out", "", "where to put y.output") //TODOOK
	_  = flag.Bool("closures", false, "closures")        //TODOOK

	lexL = func() *lex.L {
		lf, err := os.Open(lexFile)
		if err != nil {
			panic(err)
		}

		defer lf.Close()
		l, err := lex.NewL(lexFile, bufio.NewReader(lf), false, false)
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
		p, err := y.ProcessFile(fs, yaccFile, &y.Options{
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

		reports := make([][]byte, len(p.States))
		rep := out.Bytes()
		sep := []byte("\ns") // "\nstate "
		s := 0
		for i := range reports {
			e := bytes.Index(rep[s:], sep)
			if e < 0 {
				e = len(rep[s:]) - 1
			}
			reports[i] = rep[s : s+e]
			s = s + e + 1
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
			reports:   reports,
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
	l := newLexer(nil)
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
			tf.SetLinesForContent(b)
			var errs, errs2 scanner.ErrorList
			l.init(b)
			l.errHandler = func(ofs int, msg string, args ...interface{}) {
				errCnt++
				errs.Add(tf.Position(tf.Pos(ofs)), fmt.Sprintf(msg, args...))
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
	l := newLexer(nil)
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
		bsrc := []byte(src)
		f := fs.AddFile("", -1, len(bsrc))
		f.SetLinesForContent(bsrc)
		l.init(bsrc)
		for j, v := range v.toks {
			ofs, tok := l.scan()
			if g, e := ofs, v.ofs; g != e {
				nerr++
				t.Errorf("%v ofs[%d] %q(|% x|) %v %v", i, j, src, src, g, e)
			}
			if g, e := f.Position(f.Pos(ofs)).String(), v.pos; g != e {
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
	l := newLexer(nil)
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
		f.SetLinesForContent(src)
		l.init(src)
		l.errHandler = func(ofs int, msg string, arg ...interface{}) {
			se.Add(f.Position(f.Pos(ofs)), fmt.Sprintf(msg, arg...))
		}
		s.Init(f2, src, nil, 0)
		files++
		for {
			ofs, gt := l.scan()
			toks++
			glit := string(l.lit)
			pos, et, lit := s.Scan()
			position := f2.Position(pos)
			if g, e := f.Position(f.Pos(ofs)), position; g != e {
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
	_ = t.Run("States", testScannerStates) && //TODOOK
		t.Run("Bugs", testScannerBugs) &&
		t.Run("GOROOT", func(*testing.T) { testScanner(t, gorootTestFiles) })
}

func BenchmarkScanner(b *testing.B) {
	l := newLexer(nil)

	b.Run("Shootout", func(b *testing.B) {
		src, err := ioutil.ReadFile(filepath.Join(runtime.GOROOT(), "src", "go", "scanner", "scanner.go"))
		if err != nil {
			b.Fatal(err)
		}

		b.SetBytes(int64(len(src)))
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			l.init(src)
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
				l.init(src)
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
					f0.Close()
					b.Fatal(err)
				}

				sum += len(src)
				l.init(src)
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
					l := newLexer(src)
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
					l := newLexer(src)
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

func BenchmarkParser(b *testing.B) {
	l := newLexer(nil)
	p := newParser(l)

	b.Run("Shootout", func(b *testing.B) {
		src, err := ioutil.ReadFile(filepath.Join(runtime.GOROOT(), "src", "go", "parser", "parser.go"))
		if err != nil {
			b.Fatal(err)
		}

		b.SetBytes(int64(len(src)))
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			l.init(src)
			p.init(l)
			p.file()
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
				l.init(src)
				p.init(l)
				p.file()
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
					f0.Close()
					b.Fatal(err)
				}

				sum += len(src)
				l.init(src)
				p.init(l)
				p.file()
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
					l := newLexer(src)
					p := newParser(l)
					p.file()
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
					l := newLexer(src)
					p := newParser(l)
					p.file()
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
	f             *token.File
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
	l.f = f
	l.lexer.init(src)
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
		panic(fmt.Sprintf("%s: missing symbol for token %q", l.f.Position(l.f.Pos(ofs)), tok))
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
			if p.trace != nil {
				p.trace(p.yyS[n-1])
			}
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
		b.Write(p.reports[state])
		b.WriteString("----\n")
	}
	return b.String()
}

func (p *yparser) sym2str(sym *y.Symbol) string {
	nm := sym.Name
	if nm == "$end" {
		return ""
	}

	if nm[0] == '\'' {
		return nm[1:2]
	}

	if nm[0] >= 'A' && nm[0] <= 'Z' {
		if s := sym.LiteralString; s != "" {
			return s[1 : len(s)-1]
		}

		if s := p.tok2str(str2token[nm]); s != "" {
			return s
		}
	}

	return "@"
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
	l := newLexer(nil)
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
		f.SetLinesForContent(src)
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
			t.Fatalf("%s: %v", yl.f.Position(yl.f.Pos(ofs)), err)
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

func (p *parser) fail(nm string) string {
	var yl *ylex
	var states []int
	yp := newYParser(
		func(rule int) {
			if rule == yl.lbraceRule {
				yl.fixLbr()
			}
		},
		func(st int) { states = append(states, st) },
	)
	fs := token.NewFileSet()
	yl = newYlex(newLexer(nil), yp)
	f := fs.AddFile(nm, -1, len(p.l.src))
	f.SetLinesForContent(p.l.src)
	yl.init(f, p.l.src)
	yp.parse(
		func(st int) *y.Symbol {
			if ofs, s := yl.lex(); ofs <= p.ofs {
				return s
			}

			return yp.Syms["$end"]
		},
	)
	return yp.report(states[mathutil.Max(0, len(states)-12):])
}

func (p *parser) todo() {
	_, fn, fl, _ := runtime.Caller(1)
	p.err(p.ofs, "%q=%q: TODO %v:%v", p.c, p.l.lit, fn, fl) //TODOOK
}

func (p *parser) testSyntaxError() {
	dbg("%s", stack())
	switch p.c {
	case token.EOF:
		p.err(p.ofs, "%q=%q: syntax error at EOF", p.c, p.l.lit)
	default:
		lit := p.l.lit
		if len(lit) != 0 && lit[0] == '\n' {
			p.err(p.ofs, "%q=%q: syntax error at EOL", p.c, lit)
			break
		}

		p.err(p.ofs, "%q=%q: syntax error", p.c, lit)
	}
}

func testParser(t *testing.T, files []string) {
	var p *parser
	var ifile int
	var path string

	defer func() {
		if err := recover(); err != nil {
			t.Errorf("\n====\n%s%v (file %d/%d)", p.fail(path), err, ifile+1, len(files))
		}
	}()

	fs := token.NewFileSet()
	var f *token.File
	l := newLexer(nil)
	l.errHandler = func(ofs int, msg string, args ...interface{}) {
		switch {
		case len(args) == 0:
			panic(fmt.Errorf("%s: "+msg, f.Position(f.Pos(ofs))))
		default:
			panic(fmt.Errorf("%s: "+msg, append([]interface{}{f.Position(f.Pos(ofs))}, args...)...))
		}
	}
	for ifile, path = range files {
		src, err := ioutil.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}

		f = fs.AddFile(path, -1, len(src))
		f.SetLinesForContent(src)
		l.init(src)
		p = newParser(l)
		p.syntaxError = p.testSyntaxError
		p.file()
	}
}

func testParserRejectFS(t *testing.T) {
	yp := newYParser(nil, nil)
	l := newLexer(nil)
	p := newParser(l)
	for state, s := range yp0.States {
		syms, _ := s.Syms0()
		var a []string
		for _, sym := range syms {
			a = append(a, yp.sym2str(sym))
		}
		s0 := strings.Join(a, " ") + " "
		fs := yp.followSet(state)
		for _, sym := range yp.Syms {
			if !sym.IsTerminal {
				continue
			}

			if _, ok := fs[sym]; ok {
				continue
			}

			s := s0 + yp.sym2str(sym) + "@"
			l.init([]byte(s))
			p.init(l)
			ofs := -1
			p.syntaxError = func() {
				if ofs < 0 {
					ofs = p.ofs
				}
			}
			p.file()
			if ofs < 0 {
				t.Fatalf(`%d: "%s" unexpected success, final sym %q`, state, s, sym)
			}

			if g, e := ofs, len(s0); g < e {
				t.Fatalf(`Follow set %v
state %3d: %s unexpected error position, got %v expected %v
           %s^`, yp.followList(state), state, s, g+1, e+1, strings.Repeat("-", g))
			}
		}
	}
}

func TestParser(t *testing.T) {
	cover := append(gorootTestFiles, yaccCover)
	_ = t.Run("Yacc", func(t *testing.T) { testParserYacc(t, cover) }) && //TODOOK
		t.Run("GOROOT", func(t *testing.T) { testParser(t, cover) }) &&
		t.Run("RejectFollowSet", testParserRejectFS)

}
