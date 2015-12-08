exports.InputStream = function(input) {
	var pos = 0,
		line = 1,
		col = 0;

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