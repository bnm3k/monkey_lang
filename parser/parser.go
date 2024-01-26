package parser

import (
	"github.com/bnm3k/monkey_lang/ast"
	"github.com/bnm3k/monkey_lang/lexer"
	"github.com/bnm3k/monkey_lang/token"
)

type Parser struct {
	l         *lexer.Lexer
	curToken  token.Token
	peekToken token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}
	p.curToken = l.NextToken()
	p.peekToken = l.NextToken()
	return p
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	return nil
}
