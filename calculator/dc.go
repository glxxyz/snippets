package main

/*
 * Go (lang, golang) re-implementation of Bjarne Stroustrup's classic C++ 
 * desk calculator, as found in The C++ Programming Language and also here:
 * http://www.stroustrup.com/dc.c
 *
 * No guarantees offered. Constructive comments to alan@galax.xyz
 */

// The desk calculator

// includes character-level input (sec6.1.3), but
// no command line input (sec6.1.7),
// no namespaces, and
// no exceptions

// pp 107-117, sec 6.1, A Desk calculator

// uses += rather than push_back() for string
// to work around standard library bug

// No guarantees offered. Constructive comments to bs@research.att.com

/*
    program:
	END			   // END is end-of-input
	expr_list END

    expr_list:
	expression PRINT	   // PRINT is semicolon
	expression PRINT expr_list

    expression:
	expression + term
	expression - term
	term

    term:
	term / primary
	term * primary
	primary

    primary:
	NUMBER
	NAME
	NAME = expression
	- primary
	( expression )
*/

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"unicode"
)

var noOfErrors int // note: default initialized to 0

func outputError(s string) float64 {
	noOfErrors++
	_, _ = fmt.Fprintf(os.Stderr, "error: %s\n", s)
	return 1
}

type tokenValue int

const (
	NAME tokenValue = iota
	NUMBER
	END
	PLUS   = '+'
	MINUS  = '-'
	MUL    = '*'
	DIV    = '/'
	PRINT  = ';'
	ASSIGN = '='
	LP     = '('
	RP     = ')'
)

var currTok tokenValue = PRINT
var numberValue float64
var stringValue string

var reader = bufio.NewReader(os.Stdin)

func peekByte() byte {
	if bytes, err := reader.Peek(1); err == nil {
		return bytes[0]
	} else {
		return 0
	}
}

func getToken() tokenValue {
	var ch byte
	for ch, _ = reader.ReadByte(); ch != '\n' && unicode.IsSpace(rune(ch)); ch, _ = reader.ReadByte() {
		if ch == 0 {
			currTok = END
			return currTok
		}
	}

	switch ch {
	case ';',
		'\n':
		currTok = PRINT
	case '*',
		'/',
		'+',
		'-',
		'(',
		')',
		'=':
		currTok = tokenValue(ch)
	case '0', '1', '2', '3', '4',
		'5', '6', '7', '8', '9',
		'.':
		number := string(ch)
		for unicode.IsDigit(rune(peekByte())) || peekByte() == '.' {
			char, _ := reader.ReadByte()
			number += string(char)
		}
		numberValue, _ = strconv.ParseFloat(number, 64)
		currTok = NUMBER
	default: // NAME, NAME=, or error
		if unicode.IsLetter(rune(ch)) {
			stringValue = string(ch)
			for unicode.IsLetter(rune(peekByte())) {
				char, _ := reader.ReadByte()
				stringValue += string(char)
			}
			currTok = NAME
		} else {
			outputError("bad token")
			currTok = PRINT
		}
	}

	return currTok
}

var table = make(map[string]float64)

func prim(get bool) float64 { // handle primaries
	if get {
		getToken()
	}

	switch currTok {
	case NUMBER: // floating-point constant
		{
			v := numberValue
			getToken()
			return v
		}
	case NAME:
		{
			assignString := stringValue
			if getToken() == ASSIGN {
				table[assignString] = expr(true)
			}
			return table[assignString]
		}
	case MINUS: // unary minus
		return -prim(true)
	case LP:
		{
			e := expr(true)
			if currTok != RP {
				return outputError(") expected")
			}
			getToken() // eat ')'
			return e
		}
	default:
		return outputError("primary expected")
	}
}

func term(get bool) float64 { // multiply and divide
	left := prim(get)

	for {
		switch currTok {
		case MUL:
			left *= prim(true)
		case DIV:
			if d := prim(true); d != 0.0 {
				left /= d
			} else {
				return outputError("divide by 0")
			}
		default:
			return left
		}
	}
}

func expr(get bool) float64 { // add and subtract
	left := term(get)

	for {
		switch currTok {
		case PLUS:
			left += term(true)
		case MINUS:
			left -= term(true)
		default:
			return left
		}
	}
}

func main() {
	table["pi"] = 3.1415926535897932385 // insert predefined names
	table["e"] = 2.7182818284590452354

	for {
		getToken()
		if currTok == END {
			break
		}
		if currTok == PRINT {
			continue
		}
		fmt.Println(expr(false))
	}
	os.Exit(noOfErrors)
}
