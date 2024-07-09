class pipestream:
    def stream(self, show, showname, showlocal):
        if show:
            if showname:
                print(showname)
            if showlocal:
                print(showlocal)
        else:
            print("No show selected")
            return False
        return True
    
    def show(self, show, showname, showlocal):
        if show:
            if showname:
                print(showname)
            if showlocal:
                print(showlocal)
                return True
            else:
                print("No show selected")
                return False
            return True
        else:
            print("No show selected")
            return False
        