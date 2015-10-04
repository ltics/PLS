# -*- coding: utf-8 -*-

def mainloop(program, bracket_map):
    pc = 0
    tape = Tape()

    while pc < len(program):
        code = program[pc]

        if code == ">":
            tape.advance()

        elif code == "<":
            tape.devance()

        elif code == "+":
            tape.inc()

        elif code == "-":
            tape.dec()

        elif code == ".":
            # chr 将对应的ascii码转换成字符
            sys.stdout.write(chr(tape.get()))

        elif code == ",":
            # read from stdin
            # just read single character
            # ord 将对应的字符转换成ascii码
            tape.set(ord(sys.stdin.read(1)))

        elif code == "[" and tape.get() == 0:
            # Skip forward to the matching ]
            pc = bracket_map[pc]

        elif code == "]" and tape.get() != 0:
            # Skip back to the matching [
            pc = bracket_map[pc]

        pc += 1

class Tape(object):
    def __init__(self):
        self.thetape = [0]
        self.position = 0

    def get(self):
        return self.thetape[self.position]
    def set(self, val):
        self.thetape[self.position] = val
    def inc(self):
        self.thetape[self.position] += 1
    def dec(self):
        self.thetape[self.position] -= 1
    def advance(self):
        self.position += 1
        if len(self.thetape) <= self.position:
            self.thetape.append(0)
    def devance(self):
        self.position -= 1

def parse(program):
    parsed = []
    bracket_map = {}
    leftstack = []

    pc = 0
    for char in program:
        if char in ('[', ']', '<', '>', '+', '-', ',', '.'):
            parsed.append(char)

            # '['和']'需要记录下各自在整个program里的位置用来跳转
            if char == '[':
                leftstack.append(pc)
            elif char == ']':
                left = leftstack.pop()
                right = pc
                bracket_map[left] = right
                bracket_map[right] = left
            pc += 1

    return "".join(parsed), bracket_map

def run(input):
    program, map = parse(input.read())
    mainloop(program, map)

if __name__ == "__main__":
    import sys
    run(open(sys.argv[1], 'r'))
