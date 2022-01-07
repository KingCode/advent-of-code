(ns advent-of-code.-2021.day16.input
  (:require [clojure.string :refer [trim]]))

;; --- Day 16: Packet Decoder ---

;; As you leave the cave and reach open waters, you receive a 
;; transmission from the Elves back on the ship.

;; The transmission was sent using the Buoyancy Interchange 
;; Transmission System (BITS), a method of packing numeric expressions
;; into a binary sequence. Your submarine's computer has saved 
;; the transmission in hexadecimal (your puzzle input).

(def input (-> "./resources/_2021/day16/input.txt" slurp trim))

;; The first step of decoding the message is to convert the hexadecimal
;; representation into binary. Each character of hexadecimal corresponds
;; to four bits of binary data:

(def hex->bin-raw
"0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111")


;; The BITS transmission contains a single * packet * at its outermost layer 
;; which itself contains many other packets. The hexadecimal representation 
;; of this packet might encode a few extra 0 bits at the end; these are not 
;; part of the transmission and should be ignored.

;; Every packet begins with a standard header: the first three bits encode the
;; packet * version *, and the next three bits encode the packet * type ID *.
;; These two values are numbers; all numbers encoded in any packet are represented
;; as binary with the most significant bit first. For example, a version encoded 
;; as the binary sequence 100 represents the number 4.

;; Packets with type ID 4 represent a literal value. Literal value packets encode
;; a single binary number. To do this, the binary number is padded with leading
;; zeroes until its length is a multiple of four bits, and then it is broken into
;; groups of four bits. Each group is prefixed by a 1 bit except the last group,
;; which is prefixed by a 0 bit. These groups of five bits immediately follow the
;; packet header. 

(def sample-type4 "D2FE28")

;; For example, the hexadecimal string D2FE28 becomes:

;; 110100101111111000101000
;; VVVTTTAAAAABBBBBCCCCC

;; Below each bit is a label indicating its purpose:

;;      -The three bits labeled V (110) are the packet version, 6.
;;
;;      -The three bits labeled T (100) are the packet type ID, 4,
;;       which means the packet is a literal value.
;;
;;      -The five bits labeled A (10111) start with a 1 (not the last group,
;;       keep reading) and contain the first four bits of the number, 0111.
;;
;;      -The five bits labeled B (11110) start with a 1 (not the last group, 
;;       keep reading) and contain four more bits of the number, 1110.
;;
;;      -The five bits labeled C (00101) start with a 0 (last group, end of packet)
;;       and contain the last four bits of the number, 0101.
;;
;;      -The three unlabeled 0 bits at the end are extra due to the hexadecimal
;;       representation and should be ignored.

;; So, this packet represents a literal value with binary representation 011111100101, 
;; which is 2021 in decimal.

(def sample-type4-literal-bin "011111100101")

(def sample-type4-value [6 4 2021]) ;; tentative model 
                                            ;; -> [versionID, typeID, value or :op]

;; Every other type of packet (any packet with a type ID other than 4) represent 
;; an operator that performs some calculation on one or more sub-packets contained
;; within. Right now, the specific operations aren't important; focus on parsing
;; the hierarchy of sub-packets.

;; An operator packet contains one or more packets. To indicate which subsequent
;; binary data represents its sub-packets, an operator packet can use one of 
;; two modes indicated by the bit immediately after the packet header;
;; this is called the length type ID:

;;   - If the length type ID is 0, then the next 15 bits are a number that 
;;     represents the total length in bits of the sub-packets contained by 
;;     this packet.

;;   - If the length type ID is 1, then the next 11 bits are a number that 
;;     represents the number of sub-packets immediately contained by this packet.

;; Finally, after the length type ID bit and the 15-bit or 11-bit field, 
;; the sub-packets appear.

(def sample-type-length0 "38006F45291200")

;; For example, here is an operator packet (hexadecimal string 38006F45291200) 
;; with length type ID 0 that contains two sub-packets:

;; 00111000000000000110111101000101001010010001001000000000
;; VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB

;;    - The three bits labeled V (001) are the packet version, 1.
;;
;;    - The three bits labeled T (110) are the packet type ID, 6, which means
;;      the packet is an operator.
;;
;;    - The bit labeled I (0) is the length type ID, which indicates that the 
;;      length is a 15-bit number representing the number of bits in the 
;;      sub-packets.
;;
;;    - The 15 bits labeled L (000000000011011) contain the length of the 
;;      sub-packets in bits, 27.
;;
;;    - The 11 bits labeled A contain the first sub-packet, a literal value 
;;      representing the number 10.
;;
;;    - The 16 bits labeled B contain the second sub-packet, a literal value
;;      representing the number 20.

;; After reading 11 and 16 bits of sub-packet data, the total length indicated
;; in L (27) is reached, and so parsing of this packet stops.
(def sample-type-length0-value [1 6 [[6 4 10] [2 4 20]]])


(def sample-type-length1 "EE00D40C823060")

;; As another example, here is an operator packet (hexadecimal string 
;; EE00D40C823060) with length type ID 1 that contains three sub-packets:

;; 11101110000000001101010000001100100000100011000001100000
;; VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC

;;    - The three bits labeled V (111) are the packet version, 7.
;;
;;    - The three bits labeled T (011) are the packet type ID, 3, which means 
;;      the packet is an operator.
;;
;;    - The bit labeled I (1) is the length type ID, which indicates that the
;;      length is a 11-bit number representing the number of sub-packets.
;;
;;    - The 11 bits labeled L (00000000011) contain the number of sub-packets, 3.
;;
;;    - The 11 bits labeled A contain the first sub-packet, a literal value
;;      representing the number 1.
;;
;;    - The 11 bits labeled B contain the second sub-packet, a literal value
;;      representing the number 2.
;;
;;    - The 11 bits labeled C contain the third sub-packet, a literal value
;;      representing the number 3.

;; After reading 3 complete sub-packets, the number of sub-packets indicated 
;; in L (3) is reached, and so parsing of this packet stops.

(def sample-type-length1-value [7 3 [[2 4 1] [4 4 2] [1 4 3]]])

;; For now, parse the hierarchy of the packets throughout the transmission and
;; add up all of the version numbers.

;; Here are a few more examples of hexadecimal-encoded transmissions:

(def sample1 "8A004A801A8002F478")
;;    8A004A801A8002F478 
;; represents an operator packet (version 4) which contains an operator 
;; packet (version 1) which contains an operator packet (version 5) which contains
;; a literal value (version 6); this packet has a version sum of 16.

;; LAYOUT of sample1 bits:
;; 100010100000000001001010100000000001101010000000000000101111010001111000
;; VVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLVVVVVVILLLLLLLLLLLLLLLVVVTTT ^^^^
;;         Outer Packet
;; vID=4, typeID=2, lenMode=1, so 11 bits for # of sub-packets = 1 sub-packet
;;         Sub-Packet 
;; vID=1, typeID=2, lenMode=1 -> 1 sub-sub-packet 
;;         Sub Sub Packet
;; vID=5, typeID=2, lenMode=0 -> 11 bits for sub sub sub packet
;;         Sub Sub Sub Packet
;; vID=6, typeID=4 -> literal, bits = 1111 -> 15 

(def sample1-value [4 2 [[1 2 [[5 2 [[6 4 15]]]]]]])
(def sample1-vid-sum 16)

(def sample2 "620080001611562C8802118E34")
;;   620080001611562C8802118E34
;; represents an operator packet (version 3) which contains 
;; two sub-packets; each sub-packet is an operator packet that contains two 
;; literal values. This packet has a version sum of 12.

;; LAYOUT of sample2 bits:
;; 01100010000000001000000000000000000101100001000101010110001011001000100000000010
;; VVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLLLLLVVVTTT ^^^^VVVTTT ^^^^VVVTTTILLLLLLLLLLL
;;                                         ----------------------
;;                                         
;; 000100011000111000110100
;; VVVTTT ^^^^VVVTTT ^^^^          
;;
;;       Outer Packet
;; vID=3, typeID=0, lenMode=1 -> 2 sub-packets
;;       Sub Packet 1
;; VId=0, typeID=0, lenMode=0 -> 10110 = sub sub pakcets in next 22 bits 
;;       Sub Packet 1 Sub Packet 1 
;; vID=0, typeID=4, 1010 = 10,
;;       Sub Packet 1 Sub Packet 2 
;; vID=5, typeID=4, 1011 = 11
;;       <end of Sub Packet 1 sub packets bits>
;;       Sub Packet 2 
;; vID=1, typeID=0, lenMode=0 -> 2 sub-packets 
;;       Sub Packet 2 Sub Packet 1
;; vID=0, typeID=4, 1100 = 12
;;       Sub Pakcet 2 Sub Packet 2
;; vID=3, typeID=4, 1101 = 13
(def sample2-value [3 0 [[0 0 [[0 4 10] [5 4 11]]]
                         [1 0 [[0 4 12] [3 4 13]]]]])
(def sample2-vid-sum 12)

(def sample3 "C0015000016115A2E0802F182340")
;;   C0015000016115A2E0802F182340
;; has the same structure as the previous example, but the outermost packet uses
;; a different length type ID. This packet has a version sum of 23.

;; LAYOUT of sample3 bits (beginning):
;; 
;; 11000000000000010101000000000000000000010110000100010101101000101110000010000000
;; VVVTTTILLLLLLLLLLLLLLLVVVTTTILLLLLLLLLLLLLLLVVVTTT ^^^^VVVTTT ^^^^VVVTTTILLLLLLL
;;                       __________________________________________________________
;;                                             ||||||||||||||||||||||
;;     Outer Packet
;; vID=6, typeID=0, lenMode=0 -> Sub Packets within next 84 bits  
;;     Sub Packet 1
;; vID=0, typeID=0, lenMode=0 -> sub packets within next 22 bits
;;     Sub Packet 1 Sub Packt 1
;; vID=0, typeID=4, 1010 = 10
;;     Sub Packet 1 Sub Packt 2 
;; vID=6, typeID=4, 1011 = 11 
;;     Sub Packet 2
;; vID=4, tyupeID=0, lenMode=1 -> 2 sub packets  
;;     Sub Packet 2 Sub Packet 1
;; vID=7, typeID=4, 1100 = 12
;;     Sub Packet 2 Sub Packet 2
;; vID=0, typeID=4, 1101 = 13

;; LAYOUT of sample3 bits (end):
;; 00101111000110000010001101000000
;; LLLLVVVTTT ^^^^VVVTTT ^^^^
;; __________________________

(def sample3-value
  [6 0 [[0 0 [[0 4 10] [6 4 11]]]
        [4 0 [[7 4 12] [0 4 13]]]]])
(def sample3-vid-sum 23)


(def sample4 "A0016C880162017C3686B18A3D4780")
;; A0016C880162017C3686B18A3D4780
;; is an operator packet that contains an operator packet that contains an operator
;; packet that contains five literal values; it has a version sum of 31.

;; LAYOUTE of sample4 bits (begining):
;; 10100000000000010110110010001000000000010110001000000001011111000011011010000110
;; VVVTTTILLLLLLLLLLLLLLLVVVTTTILLLLLLLLLLLVVVTTTILLLLLLLLLLLVVVTTT ^^^^VVVTTT ^^^^
;;                       ----------------------------------------------------------

;;       Outer Pascket
;; vID=5, typeID=0, lenMode=0 -> sub packets within next 91 bits
;;       Sub Packet 1
;; vID=1, typeID=0, lenMode=1 -> 1 sub packet
;;       Sub Pakcet [1 1]
;; vID= 3, typeID=0, lenMode=1 -> 5 sub packets
;;       Sub Packet1 [1 1 1]
;; vID=7, typeID=4, 0110 = 6
;;       Sub Pakcet  [1 1 2]  
;; vID=6, typeID=4 0110 = 6
;;       Sub Packet [1 1 3]
;; vID=5, typeId=4, 1100 = 12
;;       Sub Packet [1 1 4]
;; vID=2, typeID=4, 1111 = 15
;;       Sub Packet [1 1 5]
;; vID=2, typeID=4, 1111 = 15
;;      (end of Outer Packet)

;; LAYOUT of sample4 bits (end):
;; 1011000110001010001111010100011110000000
;; VVVTTT ^^^^VVVTTT ^^^^VVVTTT ^^^^
;; ---------------------------------

(def sample4-value 
  [5 0 [[1 0 [[3 0 [[7 4 6]
                     [6 4 6]
                     [5 4 12]
                     [2 4 15]
                     [2 4 15]]]]]]])
(def sample4-vid-sum 31)

;; Decode the structure of your hexadecimal-encoded BITS transmission; 
;; what do you get if you add up the version numbers in all packets?

;; --- Part Two ---

;; Now that you have the structure of your transmission decoded, you can 
;; calculate the value of the expression it represents.

;; Literal values (type ID 4) represent a single number as described above.
;; The remaining type IDs are more interesting:

;;     Packets with type ID 0 are sum packets - their value is the sum of
;;     the values of their sub-packets. If they only have a single 
;;     sub-packet, their value is the value of the sub-packet.
;;
;;     Packets with type ID 1 are product packets - their value is the
;;     result of multiplying together the values of their sub-packets. 
;;     If they only have a single sub-packet, their value is the value of
;;     the sub-packet.
;;
;;     Packets with type ID 2 are minimum packets - their value is the minimum
;;     of the values of their sub-packets.
;;
;;     Packets with type ID 3 are maximum packets - their value is the maximum
;;     of the values of their sub-packets.
;;
;;     Packets with type ID 5 are greater than packets - their value is 1 if the
;;     value of the first sub-packet is greater than the value of the second
;;     sub-packet; otherwise, their value is 0. These packets always have exactly
;;     two sub-packets.
;;
;;     Packets with type ID 6 are less than packets - their value is 1 if the 
;;     value of the first sub-packet is less than the value of the second
;;     sub-packet; otherwise, their value is 0. These packets always have exactly
;;     two sub-packets.
;;
;;     Packets with type ID 7 are equal to packets - their value is 1 if the value
;;     of the first sub-packet is equal to the value of the second sub-packet;
;;     otherwise, their value is 0. These packets always have exactly two 
;;     sub-packets.

;; Using these rules, you can now work out the value of the outermost packet in 
;; your BITS transmission.

;; For example:

    ;; C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
(def part2-sample1 "C200B40A82")
(def part2-sample1-value 3)

    ;; 04005AC33890 finds the product of 6 and 9, resulting in the value 54.
(def part2-sample2 "04005AC33890")
(def part2-sample2-value 54)

    ;; 880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
(def part2-sample3 "880086C3E88112")
(def part2-sample3-value 7)

    ;; CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
(def part2-sample4 "CE00C43D881120")
(def part2-sample4-value 9)

    ;; D8005AC2A8F0 produces 1, because 5 is less than 15.
(def part2-sample5 "D8005AC2A8F0")
(def part2-sample5-value 1)

    ;; F600BC2D8F produces 0, because 5 is not greater than 15.
(def part2-sample6 "F600BC2D8F")
(def part2-sample6-value 0)

    ;; 9C005AC2F8F0 produces 0, because 5 is not equal to 15.
(def part2-sample7 "9C005AC2F8F0")
(def part2-sample7-value 0)

    ;; 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
(def part2-sample8 "9C0141080250320F1802104A08")
(def part2-sample8-value 1)

;; What do you get if you evaluate the expression represented by your hexadecimal
;; -encoded BITS transmission? 