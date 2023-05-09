class frozendict(dict):
    def __init__(self, data):
        super().__init__(data)
        self._sorted_items = tuple(sorted(self.items()))

    def __hash__(self):
        return hash(self._sorted_items)

    def __lt__(self, other):
        return self._sorted_items < other._sorted_items

    def __eq__(self, other):
        return self._sorted_items == other._sorted_items

    def __setitem__(self, key, value):
        raise ValueError("Cannot modify frozendict")

    def __delitem__(self, key):
        raise ValueError("Cannot modify frozendict")
