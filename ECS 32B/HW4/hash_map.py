from unordered_list import UnorderedList

class Entry:

    def __init__(self, key = None, value = None):
        ### private attributions
        ### only change or get by method.
        self._key = key
        self._value = value

    def setKey(self, key):
        self._key = key

    def setValue(self,value):
        self._value = value

    def getKey(self):
        return self._key

    def getValue(self):
        return self._value

class HashMap:
    def __init__(self, table_size=11, can_rehash=False):
        # TODO: Implement. Add any member variables that
        # you feel are necessary.
        self.table_size = table_size
        self.table = [UnorderedList() for _ in range(self.table_size)]
        self.can_rehash = can_rehash
        self.load_factor = 0. / self.table_size
        self.num_keys = 0

    def hash_function(self, x):
        return x % self.table_size

    def get_table_size(self):
        # TODO: Implement.
        return self.table_size

    def get_num_keys(self):
        # TODO: Implement. Must take constant time.
        return self.num_keys

    def get_load_factor(self):
        # TODO: Implement.
        return self.num_keys / self.table_size + 0.

    def insert(self, key, value):
        # TODO: Implement.
        index = self.hash_function(key)
        current_chain = self.table[index]
        found = self.__contains__(key)
        if found:
            raise ValueError("Key already mapped.")
        else:
            current_chain.add(Entry(key, value))
            self.num_keys += 1

        ### Do rehash if load factor larger than 0.5
        if self.can_rehash and self.get_load_factor() >= 0.5:
            self.reHash()

    def update(self, key, value):
        # TODO: Implement.
        index = self.hash_function(key)
        current_chain = self.table[index]
        current_node = current_chain.head
        while current_node is not None:
            if current_node.getData().getKey() == key:
                current_node.setData(Entry(key, value))
                return True
            else:
                current_node = current_node.getNext()
        return False


    def delete(self, key):
        # TODO: Implement.
        index = self.hash_function(key)
        current_chain = self.table[index]
        current_node = current_chain.head
        if current_node is not None:
            previous = None
            found = False
            while current_node is not None:
                if current_node.getData().getKey() == key:
                    found = True
                    break
                else:
                    previous = current_node
                    current_node = current_node.getNext()
            if found:
                if previous is None:
                    current_chain.head = current_node.getNext()
                else:
                    previous.setNext(current_node.getNext())
                self.num_keys -= 1

    def find(self, key):
        # TODO: Implement.
        index = self.hash_function(key)
        current_chain = self.table[index]
        current_node = current_chain.head
        while current_node is not None:
            if current_node.getData().getKey() == key:
                return current_node.getData().getValue()
            else:
                current_node = current_node.getNext()
        return None


    def print_keys(self):
        # TODO: Implement.
        for i,chain in enumerate(self.table):
            if chain.head is None:
                print("{}: None".format(i))
            else:
                current_str = str(i) + ":"
                current = chain.head
                while current is not None:
                    current_str = current_str + " " + str(current.getData().getKey())
                    current = current.getNext()
                print(current_str)

    def __contains__(self, key):
        # TODO: Implement.
        index = self.hash_function(key)
        current_chain = self.table[index]
        if current_chain.head is None:
            return False
        else:
            ## do search on this link list
            current = current_chain.head
            found = False
            while current is not None and not found:
                if current.getData().getKey() == key:
                    found = True
                else:
                    current = current.getNext()
            return found

    def reHash(self):
        nextTableSize = self.nextPrime(2 * self.table_size)
        self.table_size = nextTableSize
        newTable = [UnorderedList() for _ in range(self.table_size)]
        for chain in self.table:
            if chain.head is not None:
                current = chain.head
                while current is not None:
                    key = current.getData().getKey()
                    value = current.getData().getValue()
                    keyHash = self.hash_function(key)
                    newTable[keyHash].add(Entry(key, value))
                    current = current.getNext()
        self.table = newTable

    @staticmethod
    ### The worst-case time complexity is O(n)
    ### This may be optimized by other method.
    ### This is the most naive method to check if a number is a prime.
    def is_prime_naive_way(n):
        # Corner case
        if n <= 1:
            return False
        # Check from 2 to n-1
        for i in range(2, n):
            if (n % i) == 0:
                return False
        return True

    @staticmethod
    ### This is a better way to check if a number is prime.
    # The worst-case time complexity is O(lgn).
    # Loop times: k
    # total number: n
    # T(n) = k = (log_{2}n - 5) / 6 <= c * log(n)
    def is_prime_algorithm_way(n):
        if n <= 1:
            return False
        elif n <= 3:
            return True
        elif (n % 2) == 0 or (n % 3) == 0:
            return False
        k = 5
        while (k ** 2) <= n:
            if (n % k) == 0 or (n % (k + 2)) == 0:
                return False
            k += 6
        return True

    def nextPrime(self,x):
        ## We may assume that prime numbers are distributed evenly in the number axis (Maybe it is not right).
        ## We can add one by one to check if next number is prime.
        ## The worst-case time complexity is O(lgn). This is because T(n) = c * O(lgn). c is a number of out loop.
        while self.is_prime_algorithm_way(x+1) is False:
            x += 1
        return x + 1




if __name__ == "__main__":
    h = HashMap()
    h.insert(3, 8)
    h.insert(14, 15)
    h.insert(50, 20)
    h.insert(27, -5)
    h.insert(36, 40)
    print("=== Printing keys ===")
    h.print_keys()
    print("=== Done printing keys ===")
    print("h.get_num_keys():", h.get_num_keys())
    print("h.find(3):", h.find(3))
    print("h.find(36):", h.find(36))
    print("h.find(20):", h.find(20))
    print("3 in h:", 3 in h)
    print("36 in h:", 36 in h)
    print("40 in h:", 40 in h)  # value doesn't work
    h.delete(3)
    h.delete(36)
    h.delete(80)  # unsuccessful
    print("=== Performed three deletions (two successful) ===")
    h.print_keys()
    print("=== By default, rehashing is disabled ===")
    h.insert(11, 1)
    h.insert(22, 1)
    h.insert(33, 1)
    h.print_keys()
    print("h.get_num_keys():", h.get_num_keys())
    print("Load factor (rounded):", round(h.get_load_factor(),2))
    print("\n=== Let's try a hash table that supports rehashing ===")
    h = HashMap(5, True)
    h.insert(15, "values can be any type, by the way")
    h.insert(20, "but the keys have to be integers")
    print("===== Before insertion that causes rehash =====")
    h.print_keys()
    h.insert(26, 48)  # triggers rehashing, AFTER being inserted
    print("===== After rehashing =====")
    h.print_keys()
    print("h.get_num_keys():", h.get_num_keys())
    print("Load factor (rounded):", round(h.get_load_factor(),2))
