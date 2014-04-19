package main

import (
	"io/ioutil"
	"log"
	"os"
	"strconv"

	"sickle"
)

func init() {
	any_type.Bind(any_type_actual)
}

var (
	Rule = any_type

	any_type        = sickle.Indirect()
	any_type_actual = sickle.Choice(
		b_dict,
		b_list,
		b_string,
		b_int,
	)

	b_dict   = sickle.Seq(dict_start, dict_elements, end)
	b_list   = sickle.Seq(list_start, list_elements, end)
	b_int    = sickle.Seq(int_start, b_sint, end)
	b_string = sickle.LengthValue(sickle.Left(parseUint(b_uint), colon), sickle.Byte())

	dict_elements = sickle.Many(dict_element)
	list_elements = sickle.Many(list_element)

	dict_element = sickle.Seq(b_string, any_type)
	list_element = any_type

	b_uint = flatten(sickle.Choice(zero, whole_num))
	b_sint = flatten(sickle.Choice(zero, whole_num, neg_num))

	zero      = sickle.Ch('0')
	whole_num = sickle.Seq(pos_digit, sickle.Many(digit))
	neg_num   = sickle.Seq(neg, whole_num)

	neg       = sickle.Ch('-')
	pos_digit = sickle.ChRange('1', '9')
	digit     = sickle.ChRange('0', '9')

	int_start  = sickle.Ch('i')
	list_start = sickle.Ch('l')
	dict_start = sickle.Ch('d')
	end        = sickle.Ch('e')
	colon      = sickle.Ch(':')
)

func flatten(r sickle.Rule) sickle.Rule {
	return sickle.Action(r, func(n sickle.Node) sickle.Node {
		var ret []sickle.Node
		flatten_(&ret, n)

		n.Value = ret
		return n
	})
}

func flatten_(ret *[]sickle.Node, node sickle.Node) {
	switch v := node.Value.(type) {
	case []sickle.Node:
		for _, n := range v {
			flatten_(ret, n)
		}
	default:
		*ret = append(*ret, node)
	}
}

func parseUint(r sickle.Rule) sickle.Rule {
	return notNil(sickle.Action(r, func(n sickle.Node) sickle.Node {
		nodes := n.Value.([]sickle.Node)

		runes := make([]rune, 0, len(nodes))
		for _, node := range nodes {
			runes = append(runes, node.Value.(rune))
		}

		str := string(runes)
		num, err := strconv.ParseInt(str, 10, 64)

		if err == nil {
			n.Value = uint64(num)
		} else {
			n.Value = nil
		}

		return n
	}))
}

func notNil(r sickle.Rule) sickle.Rule {
	return sickle.Predicate(r, func(n sickle.Node) bool {
		return n.Value != nil
	})
}

func main() {
	input, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	res := sickle.Parse(input, Rule)
	log.Print(res.Failed)
}
