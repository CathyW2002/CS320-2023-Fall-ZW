class Node:
    def __init__(self, data=None, next=None):
        self.data = data
        self.next = next

class mylist:
    def __init__(self):
        self.head = None

    def append(self, data):
        if not self.head:
            self.head = Node(data)
        else:
            curr = self.head
            while curr.next:
                curr = curr.next
            curr.next = Node(data)

    def __iter__(self):
        self._curr = self.head
        return self

    def __next__(self):
        if self._curr:
            data = self._curr.data
            self._curr = self._curr.next
            return data
        raise StopIteration

    def __reversed__(self):
        prev = None
        current = self.head
        while current:
            next_node = current.next
            current.next = prev
            prev = current
            current = next_node
        self.head = prev
        return self

    def foreach(self, work):
        curr = self.head
        while curr:
            work(curr.data)
            curr = curr.next

    def rforeach(self, work):
        for data in reversed(self):
            work(data)

    @staticmethod
    def list_reverse(lst):
        reversed_list = mylist()
        curr = lst.head
        while curr:
            new_node = Node(curr.data)
            new_node.next = reversed_list.head
            reversed_list.head = new_node
            curr = curr.next
        return reversed_list
