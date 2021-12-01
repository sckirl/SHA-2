from math import sqrt

class Algorithm:
    def __init__(self):
        # hash values (first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19)
        self.hashValues = {
            "h0" : "0x6a09e667",
            "h1" : "0xbb67ae85",
            "h2" : "0x3c6ef372",
            "h3" : "0xa54ff53a",
            "h4" : "0x510e527f",
            "h5" : "0x9b05688c",
            "h6" : "0x1f83d9ab",
            "h7" : "0x5be0cd19",
        }

        # round Constants (first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311)
        self.K = ['0x428a2f98', '0x71374491', '0xb5c0fbcf', '0xe9b5dba5', 
                  '0x3956c25b', '0x59f111f1', '0x923f82a4', '0xab1c5ed5', 
                  '0xd807aa98', '0x12835b01', '0x243185be', '0x550c7dc3', 
                  '0x72be5d74', '0x80deb1fe', '0x9bdc06a7', '0xc19bf174', 
                  '0xe49b69c1', '0xefbe4786', '0x0fc19dc6', '0x240ca1cc', 
                  '0x2de92c6f', '0x4a7484aa', '0x5cb0a9dc', '0x76f988da', 
                  '0x983e5152', '0xa831c66d', '0xb00327c8', '0xbf597fc7', 
                  '0xc6e00bf3', '0xd5a79147', '0x06ca6351', '0x14292967', 
                  '0x27b70a85', '0x2e1b2138', '0x4d2c6dfc', '0x53380d13', 
                  '0x650a7354', '0x766a0abb', '0x81c2c92e', '0x92722c85', 
                  '0xa2bfe8a1', '0xa81a664b', '0xc24b8b70', '0xc76c51a3', 
                  '0xd192e819', '0xd6990624', '0xf40e3585', '0x106aa070', 
                  '0x19a4c116', '0x1e376c08', '0x2748774c', '0x34b0bcb5', 
                  '0x391c0cb3', '0x4ed8aa4a', '0x5b9cca4f', '0x682e6ff3', 
                  '0x748f82ee', '0x78a5636f', '0x84c87814', '0x8cc70208', 
                  '0x90befffa', '0xa4506ceb', '0xbef9a3f7', '0xc67178f2']

    def str2Bin(self, st: str):
        # int to 8-bit binary
        # alternate: ''.join(format(x, 'b') for x in bytearray(st, 'utf-8'))
        return ''.join(bin(ord(i))[2:].zfill(8) for i in st)

    def hex2Bin(self, hexa: str):
        # change the hexadecimal value into binary
        return bin(int(hexa, 16))[2:]

    def bin2Hex(self, binn):
        res = ""
        for i in range(0, len(binn)-3, 4):
            hexBin = int( binn[i:i+4], 2 )
            res += hex(hexBin)[-1:]

        return res

    def Rrotate(self, bin: str, n: int):
        # "rotate" the binary of given input
        n = n % len(bin)
        res = bin[-n:] + bin[:-n]
        return res

    def Rshift(self, bin: str, n: int):
        # shifts the given input, shift the characters to the right and insert 0s at the front of the list
        res = ("0"*n) + bin[:-n]
        return res if len(res) > 0 else bin

    def xor(self, first: str, second: str):
        intFirst = int(first, 2)
        intSecond = int(second, 2)

        res = intFirst ^ intSecond
        return bin(res)[2:].zfill(len(first))

    def andf(self, first: str, second: str):
        intFirst = int(first, 2)
        intSecond = int(second, 2)

        res = intFirst and intSecond

        return bin(res)[2:].zfill(len(first))

    def notf(self, bin: str):
        res = ""
        for i in bin:
            res += str(int(not i))

        return res

    def sumBin(self, li: list):
        # change each of li values (in binary) into int and sum all of them
        res = []
        for i in li:
            # interpret each character into integer values, the chars that has 0x
            # is hex, and can be interpreted as int with int(i, 16), while binary is int(i, 2)
            intBin = int(i, 16) if i[1] == "x" else int(i, 2)
            res.append(intBin)

        return bin(sum(res))[2:].zfill(len(li[0])) # return the binary shape of the sum

    def hash(self, plainText):
        def processChunks(binText):
            # copy the words and put them into a list of 32 bit binary elements
            w = [binText[i:i+32] for i in range(0, len(binText)-32, 32)]
            w += ["00000000000000000000000000000000"]*(64 - len(w))
            
            for i in range(16, 63):
                # s0 with rotation 7 and 18, and shift to 3
                s0_0 = self.Rrotate(w[i-15], 7)
                s0_1 = self.Rrotate(w[i-15], 18)
                s0_2 = self.Rshift(w[i-2], 3)
                
                s0 = self.xor(self.xor(s0_0, s0_1), s0_2)

                s1_0 = self.Rrotate(w[i-2], 17)
                s1_1 = self.Rrotate(w[i-2], 19)
                s1_2 = self.Rshift(w[i-2], 10)
                s1 = self.xor(self.xor(s1_0, s1_1), s1_2)

                # switch each binary values to its int values and add them together
                # switch it back to binary and put the value into w[i]
                # w[i] := w[i-16] + s0 + w[i-7] + s1
                w[i] = self.sumBin( [w[i-16], s0, w[i-7], s1] )

            return w

        def compression(w):
            # make a var dictionary containing the same value as hashValues 
            var = {a : self.hex2Bin(i) for a, i in zip("abcdefghijk", self.hashValues.values())}

            for i in range(63):
                S1_0 = self.Rrotate(var["e"], 6)
                S1_1 = self.Rrotate(var["e"], 11)
                S1_2 = self.Rrotate(var["e"], 25)
                S1 = self.xor( self.xor( S1_0, S1_1 ), S1_2 )
 
                # ch := (e and f) xor ((not e) and g)
                ch = self.xor( self.andf( var["e"] , var["f"] ), 
                     self.andf( self.notf( var["e"] ), var["g"] ) )

                # temp1 := h + S1 + ch + k[i] + w[i]
                temp1 = self.sumBin( [var["h"], S1, ch, self.K[i], w[i]] )

                # S0 := (a rightrotate 2) xor (a rightrotate 13) xor (a rightrotate 22)
                S0_0 = self.Rrotate(var["a"], 2)
                S0_1 = self.Rrotate(var["a"], 13)
                S0_2 = self.Rrotate(var["a"], 22)
                S0 = self.xor(self.xor(S0_0, S0_1), S0_2)

                # maj := (a and b) xor (a and c) xor (b and c)
                maj = self.xor( self.xor( self.andf( var['a'], var['b'] ), 
                                self.andf( var['a'], var['c'] )),
                                self.andf( var['b'], var['c'] ) )
                
                temp2 = self.sumBin( [S0, maj] )

                var['h'] = var['g']                                         # h = g
                var['g'] = var['f']                                         # g = f
                var['f'] = var['e']                                         # f = e
                var['e'] = self.sumBin( [var['d'], temp1] )                 # e = d + temp1
                var['d'] = var['c']                                         # d = c
                var['c'] = var['b']                                         # c = b
                var['b'] = var['a']                                         # b = a
                var['a'] = self.sumBin( [temp1, temp2] )                    # a = temp1 + temp2

            for key, v in zip(self.hashValues.keys(), var.values()):
                # h0 = h0 + a; h1 = h1 + b; etc...
                
                # convert hashValues values into binary, add them together
                h = self.hex2Bin(self.hashValues[key])
                sumB = self.sumBin( [h, v] )

                self.hashValues[key] = self.bin2Hex(sumB) # convert the values back to hex

        # call all functions and process the text
        binText = self.str2Bin( plainText ) + '1'
        k = 512 - len(binText) - 64 # L + 1 + k + 64 = 512
                                    # k = 512 - L - 64; L = len(binText)

        binText += "0"*k
        w = processChunks(binText)
        compression(w)
        res = "".join(i[2:] for i in self.hashValues.values())

        return res.upper()

if __name__ == "__main__":
    while 1:
        inp = input("input: ")
        SHA2 = Algorithm()
        print(SHA2.hash(inp))