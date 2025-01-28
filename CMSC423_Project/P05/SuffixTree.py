class Node: 
    character = ""
    nextNodes = []
    
    def __init__(self, character, nextNodes):
        self.character = character
        self.nextNodes = nextNodes
    
    def GoOn(self, substring):
        if substring == "":
            return
        flag = False
        for n in self.nextNodes:
            if substring[0] == n.character:
                flag = True
                n.GoOn(substring[1:])
        if not flag:
            NewNode = Node(substring[0], [])
            NewNode.GoOn(substring[1:])
            self.nextNodes.append(NewNode)
    
    def edges(self, edge):
        if self.nextNodes == []:
            return [edge+self.character]
        elif len(self.nextNodes) == 1:
            return self.nextNodes[0].edges(edge+self.character)
        else:
            AllEdges = [edge+self.character]
            for n in self.nextNodes:
                NewEdges = n.edges("")
                for i in NewEdges:
                    AllEdges.append(i)
            return AllEdges
                
class SuffixTree:
    StrToConvert = ""
    HeadNode = Node("", [])
    
    def __init__(self, StrToConvert):
        self.StrToConvert = StrToConvert
    
    def convert(self):
        for i in range(len(self.StrToConvert)):
            substring = self.StrToConvert[i:]
            self.HeadNode.GoOn(substring)

input = open("input", "r")
SuffixString = ""
for line in input:
    SuffixString+=line.strip()
tree = SuffixTree(SuffixString)
tree.convert()
edges = tree.HeadNode.edges("")[0:]
if edges[0] == "":
    edges = edges[1:]
for edge in edges:
    print(edge)