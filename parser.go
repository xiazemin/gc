// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"go/token"
	"runtime"
)

type parser struct {
	c          token.Token
	errHandler func(pos token.Pos, msg string, args ...interface{})
	l          *lexer
	ofs        int
	ok         bool
}

func newParser(l *lexer) *parser {
	return &parser{
		l: l,
	}
}

func (p *parser) todo() {
	_, fn, fl, _ := runtime.Caller(1)
	p.err(p.ofs, "TODO %v:%v", fn, fl)
}

func (p *parser) err(ofs int, msg string, args ...interface{}) {
	if p.errHandler != nil {
		p.errHandler(p.l.pos(ofs), msg, args...)
		return
	}

	p.l.err(p.ofs, msg, args...)
}

func (p *parser) syntaxError() {
	switch p.c {
	case token.EOF:
		p.err(p.ofs, "syntax error at EOF")
	default:
		lit := p.l.lit
		if len(lit) != 0 && lit[0] == '\n' {
			p.err(p.ofs, "syntax error at EOL")
			break
		}

		p.err(p.ofs, "syntax error")
	}
}

func (p *parser) n() token.Token {
	p.ofs, p.c = p.l.scan()
	return p.c
}

func (p *parser) parse() {
	switch p.n() {
	case token.PACKAGE:
		p.sourceFile()
	}
	if !p.ok {
		p.syntaxError()
	}
}

// SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
func (p *parser) sourceFile() {
	if p.packageClause() && p.n() == token.SEMICOLON {
		for p.c == token.IMPORT {
			p.importDecl()
			if p.n() != token.SEMICOLON {
				return
			}
		}
		for {
			switch p.n() {
			case token.CONST:
				p.constDecl()
			case token.EOF:
				p.ok = true
				return
			default:
				return
			}

			if p.c == token.SEMICOLON {
				p.n()
				continue
			}

			return
		}
	}
}

// PackageClause = "package" PackageName .
func (p *parser) packageClause() bool {
	return p.n() == token.IDENT
}

// ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
func (p *parser) importDecl() {
	p.todo()
}

// ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
func (p *parser) constDecl() {
	switch p.n() {
	case token.LPAREN:
		switch p.n() {
		case token.RPAREN:
			p.n()
		}
	}
}
