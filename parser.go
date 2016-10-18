// Copyright 2016 The GC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"go/token"
)

type parser struct {
	c0, c         token.Token
	errHandler    func(pos token.Pos, msg string, args ...interface{})
	l             *lexer
	loophack      bool
	loophackStack []bool
	ofs           int
	ok            bool
}

func newParser(l *lexer) *parser {
	return &parser{
		l: l,
	}
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

func (p *parser) n() token.Token {
	p.ofs, p.c0 = p.l.scan()
	p.c = p.c0
	switch p.c {
	case token.FOR, token.IF, token.SELECT, token.SWITCH:
		p.loophack = true
	case token.LPAREN, token.LBRACK:
		if p.loophack || len(p.loophackStack) != 0 {
			p.loophackStack = append(p.loophackStack, p.loophack)
			p.loophack = false
		}
	case token.RPAREN, token.RBRACK:
		if n := len(p.loophackStack); n != 0 {
			p.loophack = p.loophackStack[n-1]
			p.loophackStack = p.loophackStack[:n-1]
		}
	case token.LBRACE:
		if p.loophack {
			p.c = tokenBODY
			p.loophack = false
		}
	}
	return p.c
}

func (p *parser) fixLbr(tok token.Token) {
	if tok == tokenBODY {
		p.loophack = true
	}
}

// PackageClause = "package" PackageName .
func (p *parser) packageClause() {
	p.n() // "package"
	switch p.c {
	case token.IDENT:
		p.n()
	default:
		p.syntaxError()
	}
}

// ImportSpec = [ "." | PackageName ] ImportPath .
func (p *parser) importSpec() {
	switch p.c {
	case token.PERIOD:
		p.todo()
	case token.IDENT:
		p.todo()
	case token.STRING:
		p.n()
	default:
		p.syntaxError()
	}
}

// ImportDecl = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
func (p *parser) importDecl() {
	p.n() // "import"
	switch p.c {
	case token.LPAREN:
		p.n()
	more:
		switch p.c {
		case token.RPAREN:
			p.n()
		default:
			p.importSpec()
			switch p.c {
			case token.SEMICOLON:
				p.n()
				goto more
			case token.RPAREN:
				p.n()
			}
		}
	default:
		p.importSpec()
	}
}

// IdentifierList = identifier { "," identifier } .
func (p *parser) identifierList() {
	p.n() // identifier
more:
	switch p.c {
	case token.COMMA:
		p.n()
		switch p.c {
		case token.IDENT:
			p.n()
			goto more
		default:
			p.syntaxError()
		}
	}
}

func (p *parser) complitExpr() {
	switch p.c {
	case token.LBRACE:
		p.todo()
	default:
		p.expression()
	}
}

// KeyedElement = [ Key ":" ] Element .
func (p *parser) keyedElement() {
	p.complitExpr()
	switch p.c {
	case token.COLON:
		p.n()
		p.complitExpr()
	}
}

// LiteralValue = "{" [ ElementList [ "," ] ] "}" .
func (p *parser) literalValue() {
	p.n() // '{'
more:
	switch p.c {
	case token.RBRACE:
		p.n()
	default:
		p.keyedElement()
		switch p.c {
		case token.COMMA:
			p.n()
			goto more
		case token.RBRACE:
			p.n()
		}
	}
}

// Operand = Literal | OperandName | MethodExpr | "(" Expression ")" .
func (p *parser) operand() {
	switch p.c {
	case token.CHAR, token.INT, token.STRING:
		p.n()
	case token.IDENT:
		p.n()
		switch p.c {
		case token.LBRACE:
			p.literalValue()
		}
	default:
		p.todo()
	}
}

func (p *parser) argument() {
	switch p.c {
	case token.IDENT, token.STRING:
		p.expression()
	default:
		p.typ()
	}
}

// Arguments = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
func (p *parser) arguments() {
	p.n() // '('
more:
	switch p.c {
	case token.RPAREN:
		p.n()
		return
	}

	p.argument()
	switch p.c {
	case token.ELLIPSIS:
		p.todo()
	case token.RPAREN:
		p.n()
		return
	}

	switch p.c {
	case token.COMMA:
		p.n()
		goto more
	case token.RPAREN:
		p.n()
		return
	}

	goto more
}

// PrimaryExpr = Operand | Conversion | PrimaryExpr Selector | PrimaryExpr Index
//	| PrimaryExpr Slice | PrimaryExpr TypeAssertion | PrimaryExpr Arguments .
func (p *parser) primaryExpr() {
	p.operand()
more:
	switch p.c {
	case token.LPAREN:
		p.arguments()
		goto more
	case token.PERIOD:
		p.n()
		switch p.c {
		case token.LPAREN:
			p.n()
			p.typ()
			switch p.c {
			case token.RPAREN:
				p.n()
				goto more
			}

			p.syntaxError()
		case token.IDENT:
			p.n()
			goto more
		}
	case token.LBRACK:
		p.todo()
	}
}

// UnaryExpr = PrimaryExpr | unary_op UnaryExpr .
func (p *parser) unaryExpr() {
more:
	switch p.c {
	case token.ADD, token.SUB, token.NOT, token.XOR, token.MUL, token.AND, token.ARROW:
		// unary_op = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .
		p.n()
		goto more
	default:
		p.primaryExpr()
	}
}

// Expression = UnaryExpr | Expression binary_op Expression .
func (p *parser) expression() {
	p.unaryExpr()
more:
	switch p.c {
	case token.LOR:
		p.n()
		p.expression()
	case token.LAND:
		p.n()
		p.expression()
	case token.EQL, token.NEQ, token.LSS, token.LEQ, token.GTR, token.GEQ:
		p.n()
		p.expression()
	case token.ADD, token.SUB, token.OR, token.XOR:
		p.n()
		p.expression()
	case token.MUL, token.QUO, token.REM, token.SHL, token.SHR, token.AND, token.AND_NOT:
		p.n()
		p.expression()
	default:
		return
	}
	goto more
}

// ExpressionList = Expression { "," Expression } .
func (p *parser) expressionList() {
more:
	p.expression()
	switch p.c {
	case token.COMMA:
		p.n()
		goto more
	}
}

// ConstSpec = IdentifierList [ [ Type ] "=" ExpressionList ] .
func (p *parser) constSpec() {
	switch p.c {
	case token.IDENT:
		p.identifierList()
		switch p.c {
		case token.ASSIGN:
			p.n()
			p.expressionList()
		}
	default:
		p.syntaxError()
	}
}

// ConstDecl = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
func (p *parser) constDecl() {
	p.n() // "const"
	switch p.c {
	case token.LPAREN:
		p.n()
	more:
		switch p.c {
		case token.RPAREN:
			p.n()
		default:
			p.constSpec()
			switch p.c {
			case token.SEMICOLON:
				p.n()
				goto more
			case token.RPAREN:
				p.n()
			}
		}
	default:
		p.constSpec()
	}
}

// TypeName  = identifier | QualifiedIdent .
func (p *parser) typeName() {
	p.n() // identifier
	switch p.c {
	case token.PERIOD:
		p.n()
		switch p.c {
		case token.IDENT:
			p.n()
		default:
			p.syntaxError()
		}
	}
}

// FunctionType = "func" Signature .
func (p *parser) functionType() {
	p.n() // "func"
	p.signature()
}

// Type = TypeName | TypeLit | "(" Type ")" .
func (p *parser) typ() {
	switch p.c {
	case token.IDENT:
		p.typeName()
	case token.FUNC:
		p.functionType()
	case token.MUL:
		p.pointerType()
	case token.MAP:
		p.mapType()
	default:
		p.todo()
	}
}

// MapType = "map" "[" KeyType "]" ElementType .
func (p *parser) mapType() {
	p.n() // "map"
	switch p.c {
	case token.LBRACK:
		p.n()
		p.typ()
		switch p.c {
		case token.RBRACK:
			p.n()
			p.typ()
			return
		}
	}
	p.syntaxError()
}

// FieldDecl = (IdentifierList Type | AnonymousField) [ Tag ] .
func (p *parser) fieldDecl() {
	switch p.c {
	case token.MUL:
		p.todo()
	case token.IDENT:
		p.n()
		switch p.c {
		case token.IDENT:
			p.typeName()
		case token.MAP:
			p.mapType()
		case token.MUL:
			p.pointerType()
		default:
			p.todo()
		}
		switch p.c {
		case token.STRING:
			p.todo()
		}
	default:
		p.todo()
	}
}

// StructType = "struct" "{" { FieldDecl ";" } "}" .
func (p *parser) structType() {
	p.n() // "struct"
	switch p.c {
	case tokenBODY, token.LBRACE:
		lbrace := p.c
		p.n()
	more:
		switch p.c {
		case token.RBRACE:
			p.n()
			p.fixLbr(lbrace)
		default:
			p.fieldDecl()
			switch p.c {
			case token.SEMICOLON:
				p.n()
				goto more
			case token.RBRACE:
				p.n()
			}
		}
	}
}

// TypeSpec = identifier Type .
func (p *parser) typeSpec() {
	switch p.c {
	case token.IDENT:
		p.n()
		switch p.c {
		case token.STRUCT:
			p.structType()
		}
	default:
		p.syntaxError()
	}
}

// TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
func (p *parser) typeDecl() {
	p.n() // "type"
	switch p.c {
	case token.LPAREN:
		p.n()
	more:
		switch p.c {
		case token.RPAREN:
			p.n()
		default:
			p.typeSpec()
			switch p.c {
			case token.SEMICOLON:
				p.n()
				goto more
			case token.RPAREN:
				p.n()
			}
		}
	default:
		p.typeSpec()
	}
}

// PointerType = "*" BaseType .
func (p *parser) pointerType() {
	p.n() // '*'
	p.typ()
}

// ParameterDecl = [ IdentifierList ] [ "..." ] Type .
func (p *parser) parameterDecl() {
	switch p.c {
	case token.IDENT:
		p.n()
		switch p.c {
		case token.MUL:
			p.pointerType()
		case token.IDENT:
			p.typeName()
		}
	case token.MUL:
		p.pointerType()
	default:
		p.todo()
	}
}

// ParameterList = ParameterDecl { "," ParameterDecl } .
func (p *parser) parameterList() {
more:
	p.parameterDecl()
	switch p.c {
	case token.COMMA:
		p.n()
		goto more
	}
}

// Parameters = "(" [ ParameterList [ "," ] ] ")" .
func (p *parser) parameters() {
	p.n() // '('
	switch p.c {
	case token.RPAREN:
		p.n()
	default:
		p.parameterList()
		switch p.c {
		case token.COMMA:
			p.n()
		}
		switch p.c {
		case token.RPAREN:
			p.n()
		default:
			p.syntaxError()
		}
	}
}

// InterfaceType = "interface" "{" { MethodSpec ";" } "}" .
func (p *parser) interfaceType() {
	p.n() // "interface"
	switch p.c {
	case tokenBODY, token.LBRACE:
		lbrace := p.c
		p.n()
		switch p.c {
		case token.RBRACE:
			p.n()
			p.fixLbr(lbrace)
		default:
			p.todo()
		}
	}
}

// Signature = Parameters [ Result ] .
func (p *parser) signature() {
	p.parameters()
	switch p.c {
	case token.LPAREN:
		p.parameters()
	case token.IDENT:
		p.typeName()
	case token.INTERFACE:
		p.interfaceType()
	}
}

// ReturnStmt = "return" [ ExpressionList ] .
func (p *parser) returnStmt() {
	p.n() // "return"
	switch p.c {
	case token.SEMICOLON, token.RBRACE, token.CASE:
		// nop
	default:
		p.expressionList()
	}
}

func (p *parser) body() {
	p.n() // '{'
	p.statementList()
	switch p.c {
	case token.RBRACE:
		p.n()
		return
	}
	p.syntaxError()
}

// SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .
func (p *parser) simpleStmt() {
	switch p.c {
	case token.SEMICOLON, tokenBODY:
		// nop
	case token.IDENT:
		p.expression()
		p.simpleStmtPostExpr()
	default:
		p.todo()
	}
}

func (p *parser) simpleStmtPostExpr() {
	switch p.c {
	case token.COMMA:
		p.n()
		p.expressionList()
	}
	switch p.c {
	case
		token.ASSIGN,
		token.ADD_ASSIGN, token.OR_ASSIGN:
		p.assignment()
	case token.DEFINE:
		p.n()
		p.expressionList()
	}
}

func (p *parser) ifHeader() {
	p.simpleStmt()
	switch p.c {
	case token.SEMICOLON:
		p.n()
		p.simpleStmt()
	}
}

// IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
func (p *parser) ifStmt() {
	p.n() // "if"
	p.ifHeader()
	switch p.c {
	case tokenBODY:
		p.body()
		switch p.c {
		case token.ELSE:
			p.n()
			switch p.c {
			case token.IF:
				p.todo()
			case token.LBRACE:
				p.block()
			}
		}
		return
	}
	p.syntaxError()
}

// Assignment = ExpressionList assign_op ExpressionList .
func (p *parser) assignment() {
	p.n() // '=' or "+=", etc.
	p.expressionList()
}

func (p *parser) switchCase() {
	p.n() // "case"
more:
	p.argument()
	switch p.c {
	case token.COLON:
		p.n()
	case token.COMMA:
		p.n()
		goto more
	}
}

// SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .
func (p *parser) switchStmt() {
	p.n() // "switch"
	p.ifHeader()
	switch p.c {
	case tokenBODY:
		p.n()
	more:
		switch p.c {
		case token.RBRACE:
			p.n()
		case token.CASE:
			p.switchCase()
			p.statementList()
			goto more
		case token.DEFAULT:
			p.n()
			switch p.c {
			case token.COLON:
				p.n()
			default:
				p.syntaxError()
			}
			p.statementList()
			goto more
		}
	default:
		p.syntaxError()
	}
}

// Statement =
//         Declaration | LabeledStmt | SimpleStmt |
//         GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
//         FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
//         DeferStmt .
func (p *parser) statement() {
	switch p.c {
	case token.SEMICOLON, token.RBRACE, token.CASE, token.DEFAULT:
		// nop
	case token.RETURN:
		p.returnStmt()
	case token.IF:
		p.ifStmt()
	case token.IDENT:
		p.expression()
		switch p.c {
		case token.COLON:
			p.todo() // label
		default:
			p.simpleStmtPostExpr()
		}
	case token.SWITCH:
		p.switchStmt()
	default:
		p.todo()
	}
}

// StatementList = { Statement ";" } .
func (p *parser) statementList() {
more:
	p.statement()
	switch p.c {
	case token.SEMICOLON:
		p.n()
		goto more
	}
}

// Block = "{" StatementList "}" .
func (p *parser) block() {
	p.n() // '{'
	p.statementList()
	switch p.c {
	case token.RBRACE:
		p.n()
		return
	}
	p.syntaxError()
}

// MethodDecl = "func" Receiver MethodName ( Function | Signature ) .
func (p *parser) methodDecl() {
	p.parameters()
	switch p.c {
	case token.IDENT:
		p.n()
		switch p.c {
		case token.LPAREN:
			p.signature()
			switch p.c {
			case token.LBRACE:
				p.block()
			}
			return
		}
	}
	p.syntaxError()
}

// VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
func (p *parser) varSpec() {
	p.identifierList()
	switch p.c {
	case token.ASSIGN:
		p.n()
		p.expressionList()
	default:
		p.typ()
		switch p.c {
		case token.ASSIGN:
			p.n()
			p.expressionList()
		}
	}
}

// VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
func (p *parser) varDecl() {
	p.n() // "var"
	switch p.c {
	case token.LPAREN:
		p.n()
	more:
		switch p.c {
		case token.RPAREN:
			p.n()
		default:
			p.varSpec()
			switch p.c {
			case token.SEMICOLON:
				p.n()
				goto more
			case token.RPAREN:
				p.n()
			}
		}
	default:
		p.varSpec()
	}
}

// FunctionDecl = "func" FunctionName ( Function | Signature ) .
func (p *parser) functionDecl() {
	p.n() // FunctionName
	switch p.c {
	case token.LPAREN:
		p.signature()
		switch p.c {
		case token.LBRACE:
			p.block()
		}
		return
	}

	p.syntaxError()
}

// SourceFile = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
func (p *parser) parse() {
	p.n() // Make p.c valid.
	switch p.c {
	case token.PACKAGE:
		p.packageClause()
		switch p.c {
		case token.SEMICOLON:
			p.n()
		default:
			p.syntaxError()
			return
		}
	default:
		p.syntaxError()
		return
	}
more:
	switch p.c {
	case token.IMPORT:
		p.importDecl()
		switch p.c {
		case token.SEMICOLON:
			p.n()
			goto more
		}
		p.syntaxError()
		return
	}
more2:
	switch p.c {
	case token.CONST:
		p.constDecl()
	case token.FUNC:
		p.n()
		switch p.c {
		case token.LPAREN:
			p.methodDecl()
		case token.IDENT:
			p.functionDecl()
		default:
			p.syntaxError()
		}
	case token.TYPE:
		p.typeDecl()
	case token.VAR:
		p.varDecl()
	//TODO case token.EOF:
	//TODO 	p.ok = true
	//TODO	return
	default:
		p.todo()
	}
	switch p.c {
	case token.SEMICOLON:
		p.n()
		goto more2
	default:
		p.syntaxError()
	}
}
