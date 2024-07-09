class pipeshow:
    def get(self, show, showname, showfull):
        if showfull:
            return self.get_full(show, showname)
        else:
            return self.get_short(show, showname)
        
    def get_short(self, show, showname):
        try:
            show = set(name=showname)
        except Exception as e:
            return {'error': str(e)}
        try:
            show = set(name=showname)
        except Exception as e:
            return {'error': str(e)}
        try:
            show = set(name=showname)
        except Exception as e:
            return {'error': str(e)}
        try:
            show = set(name=showname)
        except Exception as e:
            return {'error': str(e)}
            
