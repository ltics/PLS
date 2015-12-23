var lexer = require('../../src/htipl/lexer');

var assert = require('assert');
describe('lexer test', function() {
	describe('input stream', function() {
		it('should parse input stream with single character', function() {
			var inputstream = lexer.InputStream("3");
			assert.equal(inputstream.peek(), "3");
			assert.equal(inputstream.next(), "3");
			assert.equal(inputstream.eof(), true);
		});
		it('should parse input stream with multi character', function() {
			var inputstream = lexer.InputStream("321");
			assert.equal(inputstream.peek(), "3");
			assert.equal(inputstream.next(), "3");
			assert.equal(inputstream.eof(), false);
			assert.equal(inputstream.peek(), "2");
			assert.equal(inputstream.next(), "2");
			assert.equal(inputstream.eof(), false);
			assert.equal(inputstream.peek(), "1");
			assert.equal(inputstream.next(), "1");
			assert.equal(inputstream.eof(), true);
		});
	});

	describe('token stream', function() {
		it('should get number token from stream', function() {
			var inputstream = lexer.InputStream("333.3");
			var tokenstream = lexer.TokenStream(inputstream);
			assert.deepEqual(tokenstream.peek(), {
				type: 'num',
				value: 333.3
			});
			assert.deepEqual(tokenstream.next(), {
				type: 'num',
				value: 333.3
			});
			assert.equal(tokenstream.eof(), true);
		});
		it('should get string token from stream', function() {
			var inputstream = lexer.InputStream("\"cleantha\" \"3\\3\"");
			var tokenstream = lexer.TokenStream(inputstream);
			assert.deepEqual(tokenstream.peek(), {
				type: 'str',
				value: "cleantha"
			});
			assert.deepEqual(tokenstream.next(), {
				type: 'str',
				value: "cleantha"
			});
			assert.equal(tokenstream.eof(), false);
			assert.deepEqual(tokenstream.peek(), {
				type: "str",
				value: "33"
			});
			assert.deepEqual(tokenstream.next(), {
				type: "str",
				value: "33"
			});
			assert.equal(tokenstream.eof(), true);
		});
		it('should get boolean token from stream', function() {
			var inputstream = lexer.InputStream("true false");
			var tokenstream = lexer.TokenStream(inputstream);
			assert.deepEqual(tokenstream.next(), {
				type: "bool",
				value: true
			});
			assert.equal(tokenstream.eof(), false);
			assert.deepEqual(tokenstream.next(), {
				type: "bool",
				value: false
			});
			assert.equal(tokenstream.eof(), true);
		});
	});
});