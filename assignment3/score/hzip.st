#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: hzip.st,v 1.7 2019-02-19 07:36:57-08 - - $"
"Quang Dang"
"qvdang"

FileStream extend [
    nl := Character nl.
    catFile [
        | count |
        count := 0.
        [self atEnd not] whileTrue: [
            | padded | 
            count := count + 1.
            padded := count printPaddedWith: Character space to: 6.
            stdout << padded << ':' << self nextLine << nl.]]]
Character extend [
    isPrint [
        ^ (Character space <= self) & (self <= $~)
    ]
    visible [
        self isPrint ifTrue: [^ self asString]
        ifFalse: [^self asInteger printStringRadix: 16]
   ]
]
"*****frequency table************"
Object subclass: ZeroArray [
    |theArray|
    ZeroArray class >> new [
        self shouldNotImplement.]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result.
    ]

    init: size [
        theArray := Array new: size
    ]

    size [
        ^ theArray size
    ]

    at: index [
        ^ theArray at: index + 1
    ]
    at: index put: value [
        ^ theArray at: index + 1 put: value
    ]

    incr: index [
        (self at:index) isNil 
            ifTrue: [self at: index put: 0].
         self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index | aBlock value: index
            value: (self at: index) ]
    ]

]

"************Tree Object*************"
Object subclass: Leaf [
    |char count isLeaf|
    char [^ char]
    count [^ count]
    isLeaf [^ isLeaf]
    Leaf class >> new [
         self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
         |result|
         result := super new.
         result setChar: aChar andCount: aCount.
         ^ result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
        isLeaf := true. 
    ]

    <= other [
        ^ (count < other count) 
        | ((count = other count) & (char <= other char))
    ]

    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream [
         (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]

    postorder: visitor [
        visitor value: char value: isLeaf.
    ]
]

Leaf subclass: Tree [
    |left right|
    left [^ left]
    right [^ right]
    
    Tree class >> new: aChar count: aCount [
        self shouldNotImplement
    ]

    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^result.
    ]

    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
    ]
    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]
    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]

    postorder: visitor [
        left postorder: visitor.
        right postorder: visitor.
        visitor value: char value: isLeaf.
    ]
]

"**************Read bits from input file*************"
Object subclass: BitStream [
    |bitIndex byte myStream|

    BitStream class >> new [
        self shouldNotImplement
    ]
    BitStream class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^result.
        ]

    init: fileStream [
        myStream := fileStream.
        bitIndex := 1.
    ]

    nextBit [
        bitIndex = 1 ifTrue: [
            byte := myStream next.
            bitIndex := 9.
        ].
        bitIndex := bitIndex - 1.
        ^ byte value bitAt: bitIndex.
    ]

    atEnd [
     ^ bitIndex = 1 and: [myStream atEnd]
    ]
]

"*************Write outBit to the output****************"

Object subclass: OutBits [
    |bitIndex currentByte myStream|
    OutBits class >> new [
        self shouldNotImplement
    ]

    OutBits class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^result.
    ]

    clearByte [
        bitIndex := 8.
        currentByte := 0.
     ]
     init: fileStream [
        myStream := fileStream.
        self clearByte.
     ]

     flushByte [
        myStream nextPutByte: currentByte.
        self clearByte.
     ]

    writeBit: bit [
        currentByte := currentByte bitAt: bitIndex put: bit.
        bitIndex := bitIndex - 1.
        bitIndex = 0 ifTrue: [self flushByte].
    ]
]
"********Main program**********"

freqTable := ZeroArray new: 257.
freqTable at: 256 put: 1.
nl := Character nl.
queue := SortedCollection new.
quit := [:status | ObjectMemory quit: status].

|argv opt inputfile outputfile readfile decompfile tree|

argv := Smalltalk arguments.

(argv size = 0 ) ifTrue:
    [stdout << 'Syntax error. "-options inputfile [outputfile]"' << nl.
    quit value: 1 ].
(argv size = 1) ifTrue: [stdout<< 'Missing input file.' << nl.
    quit value: 1.].
(argv size >= 2) ifTrue: [
    inputfile := FileStream open: (argv at: 2)
                mode: FileStream read.
    readfile := FileStream open: (argv at: 2)
                mode: FileStream read.
        (argv size = 3) ifTrue: [ 
             outputfile := FileStream open: (argv at: 3)
                                      mode: FileStream write.
        ]ifFalse: [
         outputfile := stdout.
        ].
    opt := (argv at: 1).
"*** Create a encode table ***"
    (opt ~= '-u') ifTrue: [
        encodeTable := ZeroArray new: 257.
        [inputfile atEnd not] whileTrue:[
             |refChar|
             refChar := inputfile next asInteger.
             freqTable incr: refChar.
         ].
         freqTable keysAndValuesDo: [:key :count | 
            (count notNil and: [count > 0]) ifTrue: [
                  queue add: (Leaf new: key count: count).]
         ].
        [queue size >1] whileTrue: [
            |c lchild rchild|
            lchild := queue removeFirst.
            rchild := queue removeFirst.
            c := (lchild count) + (rchild count).
            newTree := (Tree new: (lchild char) 
                             count: c 
                             left: lchild right: rchild).
            queue add: newTree.
        ].
        tree := queue removeFirst.
        tree inorder: [:char :string | 
            encodeTable at: char put: string.]
            prefix: ''.
        binaryTree :=''.
        tree postorder: [:char :isLeaf |
            isLeaf ifFalse: [
                binaryTree := binaryTree, '1'.
            ] ifTrue: [
                bin :=''.
                char = 256 ifTrue: [
                    bin :='000000001'.
                ]ifFalse: [
                    char =0 ifTrue: [
                        bin := '000000000'.
                    ]ifFalse:[
                        8 to: 1 by:-1 do: [:index |
                            bin := bin, 
                           ((char bitAt: index) printString).
                        ].
                    ].
                ].
                binaryTree := (binaryTree, '0', bin).
            ].
        ].
    ].
        inputfile close.
        (opt = '-d') ifTrue: [
             outputfile << 'Input file: ' << inputfile << nl.].
        (opt = '-t') ifTrue: [
             outputfile << 'Encode table' << nl.
             encodeTable keysAndValuesDo: [:key :count | 
                  (count notNil and: [count > 0] )ifTrue: [
                      (key <= 255) ifTrue:[
                          outputfile << key asCharacter visible 
                                     << '    '
                       ]ifFalse:[ 
                          outputfile << 'EOF' << '    '
                       ].
                          outputfile << (freqTable at: key) 
                                     << '    ' << count
                                     << nl.].].].
        (opt = '-c') ifTrue: [
            inputfile := FileStream open: (argv at: 2)
                                    mode: FileStream read.

            outBytes := OutBits new: outputfile.
            1 to: (binaryTree size) by: 1 do: [:index |
                bit := ((binaryTree at: index) asInteger) -48.
                outBytes writeBit: bit.
            ].
            outBytes writeBit: 1.
            [inputfile atEnd] whileFalse: [
                 |ordChar|
                 ordChar := inputfile next asInteger.
                 code := (encodeTable at: ordChar).
                     1 to: (code size) by: 1 do: [:index | 
                         bit := ((code at: index) asInteger) - 48.
                         outBytes writeBit: bit.].
             ].
             eof := (encodeTable at: 256).
             1 to: (eof size) by: 1 do: [:index | 
                 bits := ((eof at: index) asInteger) - 48.
                 outBytes writeBit: bit.].
                 outBytes flushByte.
         ].
         (opt = '-u') ifTrue: [
             |iseof outBytes Stack|
             iseof := false.
             outBytes := OutBits new: outputfile.
             stack := OrderedCollection new.
             inputBits := BitStream new: readfile.
             treebuilt := false.
             reconstructTree := ''.
             node := ''.

             [(inputBits atEnd not) & (iseof not)] whileTrue: [
                 treenode :=  inputBits nextBit.
                 leafchar := 0.
                 treebuilt ifFalse: [
                     (treenode = 0) ifTrue: [
                         8 to: 1 by: -1 do: [:index |
                         bit := inputBits nextBit.
                         leafchar := leafchar bitAt: index put: bit.
                     ].
                     (leafchar = 0) ifTrue: [
                          eofmarked := inputBits nextBit.
                          (eofmarked = 1) ifTrue: [
                          leafchar := leafchar bitAt: 9 put: 1.
                         ].
                     ].
                     stack add: (Leaf new: leafchar count: '').
                    ].
                    (treenode = 1) ifTrue: [
                         (stack size > 1) ifTrue: [
                              rchild := stack removeLast.
                              lchild := stack removeLast.
                              newTree := (Tree new: 0
                                         count: ''
                                         left: lchild right: rchild).
                              stack add: newTree.
                          ] 
                          ifFalse: [
                              treebuilt := true.
                              reconstructTree := stack removeLast.
                              node := reconstructTree.
                          ].
                     ].
                ] ifTrue: [
"***Decodes**"
            (node isLeaf) ifTrue: [
               ((node char) = 256) ifTrue: [
                   iseof := true.
               ] ifFalse: [
                   stdout nextPut: (Character value: (node char)).
                   node := reconstructTree.].
            ] ifFalse: [
               (treenode = 0) ifTrue: [
                    node := node left.
                    (node isLeaf) ifTrue: [
                        ((node char) = 256) ifTrue: [
                             iseof := true.
                        ]ifFalse: [ 
                            8 to: 1 by: -1 do: [:index | 
                                bit := (node char) bitAt: index.
                                outByte writeBit: bit.].
                                node := reconstructTree.
                        ].].].
             (treenode = 1) ifTrue: [
                 node := node right.
                 (node isLeaf) ifTrue: [
                     ((node char) = 256) ifTrue: [
                         iseof := true.
                     ]ifFalse: [
                         8 to: 1 by: -1 do: [:index |
                             bit := (node char) bitAt: index.
                             outByte writeBit: bit.].
                         node := reconstructTree.
                         stdout << 'Done' << nl.
                      ].].].].].].
    ].
inputfile close.
]
