exports.InputStream = function(input) {
	var pos = 0,
		line = 1,
		col = 0;

	return {
		next: next,
		peek: peek,
		eof: eof,
		croak: croak
	};

	//returns the next value and also discards it from the stream
	function next() {
		var ch = input.charAt(pos++); //先取pos 再递增pos impretive风格的糟粕啊
		if (ch == "\n") {
			line++;
			col = 0;
		} else {
			col++;
		}
		return ch;
	}

	//returns the next value but without removing it from the stream
	function peek() {
		return input.charAt(pos);
	}

	//returns true if and only if there are no more values in the stream
	function eof() {
		return peek() == "";
	}

	//does throw new Error(msg)
	function croak(msg) {
		throw new Error(msg + " (" + line + ":" + col + ")");
	}
}

exports.TokenStream = function(input) {
	var current = null;
	var keywords = ["if", "then", "else", "lambda", "λ"];
	var booleans = ["true", "false"];
	return {
		next: next,
		peek: peek,
		eof: eof,
		croak: input.croak
	};

	function is_keyword(x) {
		return (keywords.indexOf(x) > -1);
	}

	function is_boolean(x) {
		return (booleans.indexOf(x) > -1);
	}

	function is_digit(ch) {
		return /[0-9]/i.test(ch);
	}

	//start of a identifier or a keyword
	function is_id_start(ch) {
		return /[a-zλ_]/i.test(ch);
	}

	//identifier
	function is_id(ch) {
		return is_id_start(ch) || "?!-<>=0123456789".indexOf(ch) >= 0;
	}

	function is_op(ch) {
		return "+-*/%=&|<>!".indexOf(ch) >= 0;
	}

	function is_punc(ch) {
		return ".;(){}[]".indexOf(ch) >= 0;
	}

	function is_whitespace(ch) {
		return " \t\n".indexOf(ch) >= 0;
	}

	function read_while(predicate) {
		var str = "";
		while (!input.eof() && predicate(input.peek())) {
			str += input.next();
		}
		return str;
	}

	function read_number() {
		var has_dot = false;
		var number = read_while(function(ch) {
			if (ch == ".") {
				if (has_dot) {
					return false;
				}
				has_dot = true;
				return true;
			}
			return is_digit(ch);
		});
		return {
			type: "num",
			value: parseFloat(number)
		};
	}

	function read_ident() {
		var id = read_while(is_id);
		if (is_boolean(id)) {
			return {
				type: "bool",
				value: id == "true" ? true : false
			}
		}
		return {
			type: is_keyword(id) ? "kw" : "var",
			value: id
		};
	}

	//escape '\\' in input stream
	function read_escape(end) {
		var escaped = false,
			str = "";
		input.next();
		while (!input.eof()) {
			var ch = input.next();
			if (escaped) {
				str += ch;
				escaped = false;
			} else if (ch == "\\") {
				escaped = true;
			} else if (ch == end) {
				break;
			} else {
				str += ch;
			}
		}
		return str;
	}

	function read_string() {
		return {
			type: "str",
			value: read_escape('"')
		};
	}

	function skip_comment() {
		read_while(function(ch) {
			return ch != "\n";
		});
		//skip \n
		input.next();
	}

	function read_next() {
		//skip whitespaces
		read_while(is_whitespace);
		if (input.eof()) {
			return null;
		}
		var ch = input.peek();
		if (ch == "#") {
			skip_comment();
			return read_next();
		}
		if (ch == '"') {
			return read_string();
		}
		if (is_digit(ch)) {
			return read_number();
		}
		if (is_id_start) {
			return read_ident();
		}
		if (is_punc(ch)) {
			return {
				type: "punc",
				value: input.next()
			};
		}
		if (is_op(ch)) {
			//!= >=
			return {
				type: "op",
				value: read_while(is_op)
			};
		}
		input.croak("can not handle character -> " + ch);
	}
	function peek() {
		return current || (current = read_next());
	}
	function next() {
		var token = current;
		current = null;
		return token || read_next();
	}
	function eof() {
		return peek() == null;
	}
}