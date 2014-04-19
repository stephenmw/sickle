package main

import (
	"io/ioutil"
	"log"
	"os"

	"github.com/davecgh/go-spew/spew"
	"sickle"
)

func init() {
	expr.Bind(expr_actual)
}

var (
	Rule = expr

	expr        = sickle.Indirect()
	expr_actual = sickle.Choice(
		sickle.Seq(sickle.Ch('('), expr, sickle.Ch(')')),
		additive_term,
	)

	additive_term = sickle.Choice(
		sickle.Seq(multiplicative_term, sickle.Ch('+'), multiplicative_term),
		sickle.Seq(multiplicative_term, sickle.Ch('-'), multiplicative_term),
		multiplicative_term,
	)

	multiplicative_term = sickle.Choice(
		sickle.Seq(digit, sickle.Ch('*'), digit),
		sickle.Seq(digit, sickle.Ch('/'), digit),
		digit,
	)

	digit = sickle.ChRange('0', '9')
)

func main() {
	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	res := sickle.Parse(input, Rule)
	spew.Dump(res)
}
