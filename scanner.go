// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"go/token"
	"unicode/utf8"
)

// Non ASCII character classes.
const (
	classEOF = iota + 0x80
	classNonASCII
	classNext
)

const (
	minTokenToken = token.ILLEGAL
	maxTokenToken = token.VAR
)

// Additional tokens.
const (
	tokenNL   = iota + maxTokenToken + 1
	tokenLTLT // «
	tokenGTGT // »
	tokenBODY // Only for reference parser.

	nextToken
)

var (
	nlLit = []byte{'\n'}

	semiTriggerTokens = [...]bool{
		token.BREAK:       true,
		token.CHAR:        true,
		token.CONTINUE:    true,
		token.DEC:         true,
		token.FALLTHROUGH: true,
		token.FLOAT:       true,
		token.IDENT:       true,
		token.IMAG:        true,
		token.INC:         true,
		token.INT:         true,
		token.RBRACE:      true,
		token.RBRACK:      true,
		token.RETURN:      true,
		token.RPAREN:      true,
		token.STRING:      true,
	}
)

type lexer struct {
	commentHandler  func(ofs int, lit []byte)
	commentOfs      int
	errHandler      func(pos token.Pos, msg string, args ...interface{})
	errorCount      int // Number of errors encountered.
	f               *token.File
	lit             []byte
	ofs             int // Next byte offset.
	prev            token.Token
	src             []byte
	b               byte // Current byte.
	c               byte // Current class.
	noSemiInjection bool
}

func newLexer(f *token.File, src []byte) *lexer {
	l := &lexer{
		f:   f,
		src: src,
	}
	l.n()
	return l
}

func (l *lexer) init(f *token.File, src []byte) *lexer {
	l.f = f
	l.src = src
	l.ofs = 0
	l.prev = tokenNL
	l.lit = nil
	l.commentOfs = -1
	l.noSemiInjection = false
	l.n()
	return l
}

func (l *lexer) err(ofs int, msg string, args ...interface{}) {
	l.errorCount++
	if l.errHandler != nil {
		l.errHandler(l.f.Pos(ofs), msg, args...)
	}
}

// Returns class.
func (l *lexer) n() byte { // n == next
	if l.ofs == len(l.src) {
		l.c = classEOF
		l.b = 0xff // Invalid UTF-8 byte.
		l.lit = nil
		return l.c
	}

	l.b = l.src[l.ofs]
	l.ofs++
	l.c = l.b
	if l.b > 0x7f {
		l.c = classNonASCII
	} else if l.b == 0 {
		l.err(l.ofs-1, "illegal character NUL")
	}
	return l.c
}

func (l *lexer) scan() (ofs int, tok token.Token) {
skip:
	ofs, tok = l.scan0()
	if tok == token.COMMENT {
		if l.commentHandler != nil {
			end := l.ofs
			if l.c != classEOF {
				end--
			}
			l.commentHandler(ofs, l.src[ofs:end])
		}
		if l.commentOfs < 0 {
			l.commentOfs = ofs
		}
		goto skip
	}

	co := l.commentOfs
	l.commentOfs = -1
	if tok == tokenNL || tok == token.EOF {
		if p := int(l.prev); !l.noSemiInjection && p >= 0 && p < len(semiTriggerTokens) && semiTriggerTokens[l.prev] {
			if co >= 0 {
				ofs = co
			}
			l.prev = tok
			l.lit = nlLit
			if tok == token.EOF && co < 0 {
				ofs = l.ofs
			}
			return ofs, token.SEMICOLON
		}

		if tok == token.EOF {
			l.lit = nil
			return l.ofs, token.EOF
		}

		goto skip
	}

	if tok != token.ILLEGAL {
		l.prev = tok
	}
	end := l.ofs
	if l.c != classEOF {
		end--
	}
	l.lit = l.src[ofs:end]
	return ofs, tok
}

func (l *lexer) scan0() (ofs int, tok token.Token) {
skip:
	ofs = l.ofs - 1
	switch l.c {
	case '\t', '\r', ' ':
		l.n()
		goto skip
	case '\n':
		l.f.AddLine(l.ofs)
		l.n()
		return ofs, tokenNL
	case '!':
		if l.n() == '=' {
			l.n()
			return ofs, token.NEQ
		}

		return ofs, token.NOT
	case '"':
		l.n()
	more:
		switch l.c {
		case '\n', classEOF:
			l.err(ofs, "string literal not terminated")
		case '"':
			l.n()
		case '\\':
			l.n()
			switch l.c {
			case '\n':
				l.err(l.ofs-1, "unknown escape sequence")
			case '0', '1', '2', '3', '4', '5', '6', '7':
				if l.scanOctals(3) < 3 && l.stringEscFail() {
					return ofs, token.STRING
				}
			case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '"':
				l.n()
			case 'u':
				l.n()
				if l.scanHexadecimals(4) < 4 && l.stringEscFail() {
					return ofs, token.STRING
				}
			case 'U':
				l.n()
				if l.scanHexadecimals(8) < 8 && l.stringEscFail() {
					return ofs, token.STRING
				}
			case 'x':
				l.n()
				if l.scanHexadecimals(2) < 2 && l.c == classEOF {
					l.err(l.ofs, "escape sequence not terminated")
				}
			case classEOF:
				l.err(l.ofs, "escape sequence not terminated")
			default:
				l.err(l.ofs-1, "unknown escape sequence")
				l.skip()
			}
			goto more
		default:
			l.skip()
			goto more
		}
		return ofs, token.STRING
	case '%':
		if l.n() == '=' {
			l.n()
			return ofs, token.REM_ASSIGN
		}

		return ofs, token.REM
	case '&':
		switch l.n() {
		case '^':
			if l.n() == '=' {
				l.n()
				return ofs, token.AND_NOT_ASSIGN
			}

			return ofs, token.AND_NOT
		case '=':
			l.n()
			return ofs, token.AND_ASSIGN
		case '&':
			l.n()
			return ofs, token.LAND
		}

		return ofs, token.AND
	case '\'':
		l.n()
		switch l.c {
		case '\n', classEOF:
			l.err(ofs, "rune literal not terminated")
		case '\'':
			l.err(ofs, "illegal rune literal")
			l.n()
			return ofs, token.CHAR
		case '\\':
			l.n()
			switch l.c {
			case '\n':
				l.err(l.ofs-1, "unknown escape sequence")
				return ofs, token.CHAR
			case '0', '1', '2', '3', '4', '5', '6', '7':
				if l.scanOctals(3) < 3 {
					return l.charEscFail(ofs)
				}
			case 'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'':
				l.n()
			case 'u':
				l.n()
				if l.scanHexadecimals(4) < 4 {
					return l.charEscFail(ofs)
				}
			case 'U':
				l.n()
				if l.scanHexadecimals(8) < 8 {
					return l.charEscFail(ofs)
				}
			case 'x':
				l.n()
				if l.scanHexadecimals(2) < 2 {
					return l.charEscFail(ofs)
				}
			case classEOF:
				l.err(l.ofs, "escape sequence not terminated")
				return ofs, token.CHAR
			default:
				l.err(l.ofs-1, "unknown escape sequence")
				l.skip()
				return ofs, token.CHAR
			}
		default:
			l.skip()
		}
		switch l.c {
		case '\n', classEOF:
			l.err(ofs, "rune literal not terminated")
		case '\\':
			l.err(l.ofs, "escape sequence not terminated")
			l.n()
		case '\'':
			l.n()
		default:
			l.err(ofs, "rune literal not terminated")
			l.skip()
		}
		return ofs, token.CHAR
	case '(':
		l.n()
		return ofs, token.LPAREN
	case ')':
		l.n()
		return ofs, token.RPAREN
	case '*':
		if l.n() == '=' {
			l.n()
			return ofs, token.MUL_ASSIGN
		}

		return ofs, token.MUL
	case '+':
		switch l.n() {
		case '=':
			l.n()
			return ofs, token.ADD_ASSIGN
		case '+':
			l.n()
			return ofs, token.INC
		}

		return ofs, token.ADD
	case ',':
		l.n()
		return ofs, token.COMMA
	case '-':
		switch l.n() {
		case '=':
			l.n()
			return ofs, token.SUB_ASSIGN
		case '-':
			l.n()
			return ofs, token.DEC
		}

		return ofs, token.SUB
	case '.':
		l.n()
		switch l.c {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			l.scanDecimals()
			return ofs, l.scanExponent()
		case '.':
			switch l.n() {
			case '.':
				l.n()
				return ofs, token.ELLIPSIS
			default:
				return ofs, token.ILLEGAL
			}
		default:
			return ofs, token.PERIOD
		}
	case '/':
		switch l.n() {
		case '/':
			for l.n() != '\n' && l.c != classEOF {
			}
			return ofs, token.COMMENT
		case '*':
			var hasNL bool
			for l.n(); l.c != classEOF; l.n() {
				switch l.c {
				case '\n':
					hasNL = true
					l.f.AddLine(l.ofs)
				case '*':
				more2:
					switch l.n() {
					case '*':
						goto more2
					case '\n':
						hasNL = true
						l.f.AddLine(l.ofs)
					case '/':
						l.n()
						if hasNL {
							if l.commentHandler != nil {
								end := l.ofs
								if l.c != classEOF {
									end--
								}
								l.commentHandler(ofs, l.src[ofs:end])
							}
							return ofs, tokenNL
						}

						return ofs, token.COMMENT
					}
				}
			}
			l.err(ofs, "comment not terminated")
			return ofs, token.COMMENT
		case '=':
			l.n()
			return ofs, token.QUO_ASSIGN
		default:
			return ofs, token.QUO
		}
	case '0':
		n := l.scanOctals(-1)
		switch l.c {
		case '.':
			l.n()
			l.scanDecimals()
			return ofs, l.scanExponent()
		case '8', '9':
			l.scanDecimals()
			switch l.c {
			case '.':
				l.n()
				l.scanDecimals()
				return ofs, l.scanExponent()
			case 'e', 'E':
				return ofs, l.scanExponent()
			case 'i':
				l.n()
				return ofs, token.IMAG
			default:
				l.err(ofs, "illegal octal number")
			}
		case 'e', 'E':
			return ofs, l.scanExponent()
		case 'i':
			l.n()
			return ofs, token.IMAG
		case 'x', 'X':
			if n != 1 {
				break
			}

			l.n()
			if l.scanHexadecimals(-1) == 0 {
				l.err(ofs, "illegal hexadecimal number")
			}
		}

		return ofs, token.INT
	case '1', '2', '3', '4', '5', '6', '7', '8', '9':
		l.scanDecimals()
		switch l.c {
		case '.':
			l.n()
			l.scanDecimals()
			return ofs, l.scanExponent()
		case 'e', 'E':
			return ofs, l.scanExponent()
		case 'i':
			l.n()
			return ofs, token.IMAG
		}
		return ofs, token.INT
	case ':':
		if l.n() == '=' {
			l.n()
			return ofs, token.DEFINE
		}

		return ofs, token.COLON
	case ';':
		l.n()
		return ofs, token.SEMICOLON
	case '<':
		switch l.n() {
		case '<':
			if l.n() == '=' {
				l.n()
				return ofs, token.SHL_ASSIGN
			}

			return ofs, token.SHL
		case '=':
			l.n()
			return ofs, token.LEQ
		case '-':
			l.n()
			return ofs, token.ARROW
		}

		return ofs, token.LSS
	case '=':
		if l.n() == '=' {
			l.n()
			return ofs, token.EQL
		}

		return ofs, token.ASSIGN
	case '>':
		switch l.n() {
		case '>':
			if l.n() == '=' {
				l.n()
				return ofs, token.SHR_ASSIGN
			}

			return ofs, token.SHR
		case '=':
			l.n()
			return ofs, token.GEQ
		}

		return ofs, token.GTR
	case '[':
		l.n()
		return ofs, token.LBRACK
	case ']':
		l.n()
		return ofs, token.RBRACK
	case '^':
		if l.n() == '=' {
			l.n()
			return ofs, token.XOR_ASSIGN
		}

		return ofs, token.XOR
	case '`':
	more3:
		switch l.n() {
		case '`':
			l.n()
			return ofs, token.STRING
		case '\n':
			l.f.AddLine(l.ofs)
		case classEOF:
			l.err(ofs, "raw string literal not terminated")
			return ofs, token.STRING
		}
		goto more3
	case 'b':
		if l.n() == 'r' && l.n() == 'e' && l.n() == 'a' && l.n() == 'k' && !isIdentNext(l.n()) {
			return ofs, token.BREAK
		}

		return ofs, l.scanIdent()
	case 'c':
		switch l.n() {
		case 'a':
			if l.n() == 's' && l.n() == 'e' && !isIdentNext(l.n()) {
				return ofs, token.CASE
			}
		case 'h':
			if l.n() == 'a' && l.n() == 'n' && !isIdentNext(l.n()) {
				return ofs, token.CHAN
			}
		case 'o':
			if l.n() == 'n' {
				switch l.n() {
				case 's':
					if l.n() == 't' && !isIdentNext(l.n()) {
						return ofs, token.CONST
					}
				case 't':
					if l.n() == 'i' && l.n() == 'n' && l.n() == 'u' && l.n() == 'e' && !isIdentNext(l.n()) {
						return ofs, token.CONTINUE
					}
				}
			}
		}

		return ofs, l.scanIdent()
	case 'd':
		if l.n() == 'e' && l.n() == 'f' {
			switch l.n() {
			case 'a':
				if l.n() == 'u' && l.n() == 'l' && l.n() == 't' && !isIdentNext(l.n()) {
					return ofs, token.DEFAULT
				}
			case 'e':
				if l.n() == 'r' && !isIdentNext(l.n()) {
					return ofs, token.DEFER
				}
			}
		}

		return ofs, l.scanIdent()
	case 'e':
		if l.n() == 'l' && l.n() == 's' && l.n() == 'e' && !isIdentNext(l.n()) {
			return ofs, token.ELSE
		}

		return ofs, l.scanIdent()
	case 'f':
		switch l.n() {
		case 'a':
			if l.n() == 'l' && l.n() == 'l' && l.n() == 't' && l.n() == 'h' && l.n() == 'r' && l.n() == 'o' && l.n() == 'u' && l.n() == 'g' && l.n() == 'h' && !isIdentNext(l.n()) {
				return ofs, token.FALLTHROUGH
			}
		case 'o':
			if l.n() == 'r' && !isIdentNext(l.n()) {
				return ofs, token.FOR
			}
		case 'u':
			if l.n() == 'n' && l.n() == 'c' && !isIdentNext(l.n()) {
				return ofs, token.FUNC
			}
		}

		return ofs, l.scanIdent()
	case 'g':
		if l.n() == 'o' {
			if !isIdentNext(l.n()) {
				return ofs, token.GO
			}

			if l.c == 't' && l.n() == 'o' && !isIdentNext(l.n()) {
				return ofs, token.GOTO
			}
		}

		return ofs, l.scanIdent()
	case 'i':
		switch l.n() {
		case 'f':
			if !isIdentNext(l.n()) {
				return ofs, token.IF
			}
		case 'm':
			if l.n() == 'p' && l.n() == 'o' && l.n() == 'r' && l.n() == 't' && !isIdentNext(l.n()) {
				return ofs, token.IMPORT
			}
		case 'n':
			if l.n() == 't' && l.n() == 'e' && l.n() == 'r' && l.n() == 'f' && l.n() == 'a' && l.n() == 'c' && l.n() == 'e' && !isIdentNext(l.n()) {
				return ofs, token.INTERFACE
			}
		}

		return ofs, l.scanIdent()
	case 'm':
		if l.n() == 'a' && l.n() == 'p' && !isIdentNext(l.n()) {
			return ofs, token.MAP
		}

		return ofs, l.scanIdent()
	case 'p':
		if l.n() == 'a' && l.n() == 'c' && l.n() == 'k' && l.n() == 'a' && l.n() == 'g' && l.n() == 'e' && !isIdentNext(l.n()) {
			return ofs, token.PACKAGE
		}

		return ofs, l.scanIdent()
	case 'r':
		switch l.n() {
		case 'a':
			if l.n() == 'n' && l.n() == 'g' && l.n() == 'e' && !isIdentNext(l.n()) {
				return ofs, token.RANGE
			}
		case 'e':
			if l.n() == 't' && l.n() == 'u' && l.n() == 'r' && l.n() == 'n' && !isIdentNext(l.n()) {
				return ofs, token.RETURN
			}
		}

		return ofs, l.scanIdent()
	case 's':
		switch l.n() {
		case 'e':
			if l.n() == 'l' && l.n() == 'e' && l.n() == 'c' && l.n() == 't' && !isIdentNext(l.n()) {
				return ofs, token.SELECT
			}
		case 't':
			if l.n() == 'r' && l.n() == 'u' && l.n() == 'c' && l.n() == 't' && !isIdentNext(l.n()) {
				return ofs, token.STRUCT
			}
		case 'w':
			if l.n() == 'i' && l.n() == 't' && l.n() == 'c' && l.n() == 'h' && !isIdentNext(l.n()) {
				return ofs, token.SWITCH
			}
		}

		return ofs, l.scanIdent()
	case 't':
		if l.n() == 'y' && l.n() == 'p' && l.n() == 'e' && !isIdentNext(l.n()) {
			return ofs, token.TYPE
		}

		return ofs, l.scanIdent()
	case 'v':
		if l.n() == 'a' && l.n() == 'r' && !isIdentNext(l.n()) {
			return ofs, token.VAR
		}

		return ofs, l.scanIdent()
	case '{':
		l.n()
		return ofs, token.LBRACE
	case '|':
		switch l.n() {
		case '=':
			l.n()
			return ofs, token.OR_ASSIGN
		case '|':
			l.n()
			return ofs, token.LOR
		}

		return ofs, token.OR
	case '}':
		l.n()
		return ofs, token.RBRACE
	case classEOF:
		return ofs, token.EOF
	default:
		if l.b == 0xc2 && l.ofs < len(l.src) { // {"«","»"}[0].
			l.n()
			switch l.b {
			case 0xab:
				l.n()
				return ofs, tokenLTLT
			case 0xbb:
				l.n()
				return ofs, tokenGTGT
			}
		}

		if l.c >= 'a' && l.c <= 'z' || l.c >= 'A' && l.c <= 'Z' || l.c == '_' || l.c == classNonASCII {
			l.n()
			for l.c >= 'a' && l.c <= 'z' || l.c >= 'A' && l.c <= 'Z' || l.c == '_' || l.c >= '0' && l.c <= '9' || l.c == classNonASCII {
				l.n()
			}
			return ofs, token.IDENT
		}

		switch {
		case l.b < ' ':
			l.err(ofs, "illegal character %U", l.skip())
		default:
			l.err(ofs, "illegal character %#U", l.skip())
		}
		return ofs, token.ILLEGAL
	}
}

func (l *lexer) scanOctals(max int) (n int) {
	for max != 0 && l.c >= '0' && l.c <= '7' {
		l.n()
		n++
		max--
	}
	return n
}

func (l *lexer) scanDecimals() {
	for l.c >= '0' && l.c <= '9' {
		l.n()
	}
}

func (l *lexer) scanExponent() token.Token {
	switch l.c {
	case 'e', 'E':
		l.n()
		switch l.c {
		case '+', '-':
			l.n()
		}
		l.scanDecimals()
	}
	switch l.c {
	case 'i':
		l.n()
		return token.IMAG
	}

	return token.FLOAT
}

func (l *lexer) scanHexadecimals(max int) (n int) {
	for max != 0 && (l.c >= '0' && l.c <= '9' || l.c >= 'a' && l.c <= 'f' || l.c >= 'A' && l.c <= 'F') {
		l.n()
		n++
		max--
	}
	return n
}

func isIdentNext(c byte) bool {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9' || c == classNonASCII
}

func (l *lexer) scanIdent() token.Token {
	for l.c >= 'a' && l.c <= 'z' || l.c >= 'A' && l.c <= 'Z' || l.c == '_' || l.c >= '0' && l.c <= '9' || l.c == classNonASCII {
		l.n()
	}
	return token.IDENT
}

func (l *lexer) skip() rune {
	if c := l.c; c < 0x80 {
		l.n()
		return rune(c)
	}

	if l.c == classEOF {
		return -1
	}

	r, sz := utf8.DecodeRune(l.src[l.ofs-1:])
	l.ofs += sz - 1
	l.n()
	return r
}

func (l *lexer) stringEscFail() bool {
	switch l.c {
	case '\n':
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.c)
	case '"':
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.c)
		l.n()
		return true
	case '\\':
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.c)
		fallthrough
	case classEOF:
		l.err(l.ofs, "escape sequence not terminated")
	default:
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.skip())
	}
	return false
}

func (l *lexer) charEscFail(ofs int) (int, token.Token) {
	switch l.c {
	case '\n':
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.c)
	case '\\':
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.c)
		l.n()
		fallthrough
	case classEOF:
		l.err(l.ofs, "escape sequence not terminated")
	default:
		l.err(l.ofs-1, "illegal character %#U in escape sequence", l.skip())
	}
	return ofs, token.CHAR
}

func (l *lexer) pos(ofs int) token.Pos           { return l.f.Pos(ofs) }
func (l *lexer) position(ofs int) token.Position { return l.f.Position(l.pos(ofs)) }
