class Node(object):
    def __init__(self, val):
        self.val = val
        self.next = None


def makeCircle(l):
    start = Node(l[0])
    prev = start
    cur = None
    for v in l[1:]:
        cur = Node(v)
        prev.next = cur
        prev = cur
    cur.next = start
    return start


def makeIndex(n):
    start = n
    cur = n.next
    d = {start.val: start}
    while cur != start:
        d[cur.val] = cur
        cur = cur.next

    return d


def doTurn(c):
    current = c.val
    takeStart = c.next
    takeEnd = takeStart.next.next
    takeList = [takeStart.val, takeStart.next.val, takeStart.next.next.val]
    target = destination(current - 1, takeList)
    targetNode = index[target]
    targetNext = targetNode.next
    cups.next = takeEnd.next
    targetNode.next = takeStart
    takeEnd.next = targetNext
    return c.next


def destination(j, l):
    if j < lowest:
        return destination(highest, l)
    elif j in l:
        return destination(j - 1, l)
    else:
        return j


starting = [9, 5, 2, 4, 3, 8, 7, 1, 6] + list(range(10, 1000001))
cups = makeCircle(starting)
index = makeIndex(cups)
lowest = min(starting)
highest = max(starting)

for i in range(0, 10000000):
    cups = doTurn(cups)

cupOne = index[1].next
print(cupOne.val * cupOne.next.val)
