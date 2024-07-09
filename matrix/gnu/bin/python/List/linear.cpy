class linear:
    def get(self, composite, value, index):
        return composite.get(value, index)
    
    def set(self, composite, value, index):
        composite[value] = index
        return composite
    
    def remove(self, composite, value, index):
        del composite[value]
        return composite
    
    def clear(self, composite):
        composite.clear()
        return composite
    