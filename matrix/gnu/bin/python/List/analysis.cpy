class analysis:
    def get(self, tailored, pushed):
        if tailored:
            return self.tailored_analysis(pushed)
        else:
            return self.default_analysis(pushed)
        
    def tailored_analysis(self, pushed):
        if len(pushed) == 1:
            return self.tailored_analysis_1(pushed)
        elif len(pushed) == 2:
            return self.tailored_analysis_2(pushed)
        else:
            return self.tailored_analysis_3(pushed)
        
    def default_analysis(self, pushed):
        if len(pushed) == 1:
            return self.default_analysis_1(pushed)
        elif len(pushed) == 2:
            return self.default_analysis_2(pushed)
        else:
            return self.default_analysis_3(pushed)
        
        