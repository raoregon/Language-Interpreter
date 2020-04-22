#! /usr/bin/env python3
"""
//assume in the command prompt the user has entered the argument, tl.txt, the name of
//the text file that contains a TL program in it
create sList, an empty list of Stmt objects
create symTable, an empty symbol table
for each line of statement in tl.txt:
    parse the line to an Stmt object //possibly with satellite Expr objects
    add the Stmt object to sList
    if this line is labeled:
        add (label, current line number) mapping to symTable
evaluate sList with symTable
"""
import fileinput
import sys

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self, op1, operator, op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1
        else:
            return self.op1 + " " + self.operator + " " + self.op2

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        op1 = self.op1
        op2 = self.op2
        #print("this")
        #print(op1)
        #print(op2)

        if is_number(str(op1)):
            symTable[op1] = float(op1)
        if is_number(str(op2)):
            symTable[op2] = float(op2)

        if self.operator == "var":
            return symTable[op1]
        elif self.operator == "+":
            return symTable[op1] + symTable[op2]
        elif self.operator == "-":
            return symTable[op1] - symTable[op2]
        elif self.operator == "*":
            return symTable[op1] * symTable[op2]
        elif self.operator == "/":
            return symTable[op1] / symTable[op2]
        elif self.operator == "<":
            return symTable[op1] < symTable[op2]
        elif self.operator == ">":
            return symTable[op1] > symTable[op2]
        elif self.operator == "<=":
            return symTable[op1] <= symTable[op2]
        elif self.operator == ">=":
            return symTable[op1] >= symTable[op2]
        elif self.operator == "==":
            return symTable[op1] == symTable[op2]
        elif self.operator == "!=":
            return symTable[op1] != symTable[op2]
        else:
            return 0

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs):
        self.keyword = keyword
        self.exprs = exprs

    def __str__(self):
        others = ""
        for exp in self.exprs:
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable):
        # print(symTable)
        exprs = self.exprs

        if self.keyword == 'let':
            if len(exprs) > 3:
                internal = Expr(exprs[2], exprs[3], exprs[4]).eval(symTable)
                exprs[2] = str(internal)

            if str(exprs[2]).isdigit():
                symTable[exprs[0]] = float(exprs[2])
            else:
                while not is_number(str(exprs[2])):
                    exprs[2] = symTable[exprs[2]]

                if is_number(str(exprs[2])):
                    symTable[exprs[0]] = float(exprs[2])
        elif self.keyword == 'print':
            if len(exprs) == 1:
                for expressions in exprs:
                    if expressions in symTable:
                        print(symTable[expressions])
                    else:
                        if '"' in expressions:
                            express = expressions.strip('"')
                            print(express)
                        else:
                            print(expressions)
            elif len(exprs) > 1:
                finalPrint = ""
                for expressions in exprs:
                    if expressions in symTable:
                        finalPrint += str(symTable[expressions]) + " "
                    else:
                        if '"' in expressions:
                            express = expressions.strip('"')
                            print(express)
                        else:
                            print(expressions)
                print(finalPrint[:-1])
        elif self.keyword == 'input':
            for expressions in exprs:
                    newInput = input()
                    if str(newInput).isdigit():
                        newInput = float(newInput)
                        symTable[expressions] = newInput
                    else:
                        sys.exit('Illegal or missing input')
        elif self.keyword == 'if':
            label = exprs[4]
            if Expr(exprs[0], exprs[1], exprs[2]).eval(symTable):
                #print("check")
                symTable["sListLineCount"] = int(symTable.get(label))
                #print(symTable["sListLineCount"])
            else:
                symTable["sListLineCount"] = int(symTable["sListLineCount"] + 1)
        if self.keyword != 'if':
            symTable["sListLineCount"] = int(symTable["sListLineCount"] + 1)


def main():

    symTable = {}
    symTable["sList"] = []
    symTable["sListLineCount"] = 0
    sList = symTable["sList"]

    # just a counter so we can see what line we're on when we throw errors
    lineCount = 0

    # Takes in our .txt file, and for each line in the file, we run the for loop:
    for line in fileinput.input(sys.argv[1:]):

        # lines.strip() takes the line as a string and throws it into a list called "lines"
        lines = line.strip()

        # takes the lines and turns them into a list of each string
        # ie let x = 5 becomes ['let', 'x', '=', '5']
        stringList = lines.split()

        if ':' in lines:
            symTable[stringList[0].strip(":")] = lineCount
            stringList.remove(stringList[0])

        # sets keyword to the first item in our stringList list
        keyword = stringList[0]

        # creates an empty array that we will fill in depending on the length of the expression
        expression = []
        # creates an array of legal keywords
        legalKeywords = ["let", "if", "print", "input"]

        # check to make sure keyword is spelled correctly
        if keyword not in legalKeywords:
            sys.exit('Syntax error on line {}'.format(lineCount+1))

        # now that we know the keyword, we get rid of it
        stringList.remove(stringList[0])

        # check if its a print line, if it is, adjust so prints properly
        if keyword == "print" and len(stringList) > 1:
            # print(stringList)
            count = 0
            indexNumber = -1
            partialString = ""

            while count <= len(stringList) - 1:

                if '",' in stringList[count] and indexNumber <= 0:
                    partialString += (stringList[count].strip('",'))
                    stringList.remove(stringList[count])

                    stringList.insert(count, partialString)
                    count += 2
                elif '",' in stringList[count]:
                    partialString += (stringList[count].strip('",'))
                    stringList.remove(stringList[count])

                    if indexNumber >= 0:
                        stringList.insert(indexNumber,partialString)
                    count += 2
                elif '"' in stringList[count] and indexNumber >= 0:
                    partialString += (stringList[count].strip('"'))
                    stringList.remove(stringList[count])
                    stringList.insert(indexNumber, partialString)
                    count += 1
                elif '"' in stringList[count] and count+1 == len(stringList):
                    partialString += (stringList[count].strip('"'))
                    stringList.remove(stringList[count])
                    stringList.insert(indexNumber, partialString)
                    count += 1
                elif '"' in stringList[count]:
                    partialString += (stringList[count].strip('"')) + " "
                    stringList.remove(stringList[count])
                    indexNumber = count
                elif '"' not in stringList[count] and ',' in stringList[count]:
                    stringList[count] = stringList[count].strip(' ,')
                    count += 1
                elif '"' not in stringList[count]:
                    count += 1
        while '' in stringList:
            stringList.remove('')

        # now we add the rest of the strings into expression
        for strings in stringList:
            expression.append(strings)

        # our statement has been built, so now we add it to the list of statements
        statement = Stmt(keyword, expression)
        sList.append(statement)

        lineCount += 1

    count = 0
    while count < len(symTable["sList"]):

        sListLineCount = symTable["sListLineCount"]
        #print(sListLineCount)
        sList[sListLineCount].perform(symTable)

        count = int(sListLineCount) + 1


if __name__ == "__main__":
    main()

