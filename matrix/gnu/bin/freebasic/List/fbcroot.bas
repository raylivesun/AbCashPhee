declare function fbcroot (byref fbc as string, byref root as string, _ 
byref sorted as string, byval mo as string) as string

type fbc
     root as string
     sorted as string
     mo as string
end type 

#if 0
'============================================================================================================================
Property List.IsTracked As Byte : If  bTracking=1 Then : Return 1 : Else :  Return 0 : End If  : End Property
Property List.IsTracked(uB As uByte) As Byte : If  uB=1 Or uB=0 Then : bTracking=uB : Return 1 : Else :  Return 0 : End If  : End Property
Property List.IsTagFree As Byte : Dim i As Byte : If pNode->pBranchLastNode<>0 And pNode->Tag0<>LIST_RES Then : Return 1 : Else : For i=0 To MAX_COLS : If TrackTrace(i)=pNode Then : Return 1 : End If : Next i : Return 0 : End If  : End Property

Property List.Aside As Byte : Return this.Aside(1) : End Property
Property List.Aside(by As Byte) As Byte
    If by > MAX_ASIDE Or By < 1 Then : Return 0 : End If :' If bNFTrackedOut=1 Then : bAlrdyTracked=0 : This.TrackSecure : bNFTrackedOut=0 : End If  ' If bTracking=1 Then : bAlrdyTracked=0 : tthis.TrackCompute : End If 
    this.pFirstNode->BranchCount = this.uCount : this.pFirstNode->pBranchLastNode = this.pLastNode :  Lcontext(by).uBIsTracked=bTracking
    Lcontext(by).pNode=pNode : Lcontext(by).pFirstNode=pFirstNode : Lcontext(by).pLastNode=pLastNode : Lcontext(by).uCount=this.uCount : Lcontext(by).LcHashTag=pNode->Tag0 : Lcontext(by).bLcHashLen=this.bHashLen
    Return 1
End Property
Property List.Recover As Byte : Return this.Recover(1) : End Property
Property List.Recover(by As Byte) As Byte
    If by > MAX_ASIDE Or By < 1 Then : Return 0 : End If ': If bNFTrackedOut=1 Then : bAlrdyTracked=0 : This.TrackSecure : bNFTrackedOut=0 : End If 
    this.pFirstNode->BranchCount = this.uCount : this.pFirstNode->pBranchLastNode = this.pLastNode : bTracking=Lcontext(by).uBIsTracked
    pNode=Lcontext(by).pNode : pFirstNode=Lcontext(by).pFirstNode : this.bHashLen=Lcontext(by).bLcHashLen
    pLastNode=pFirstNode->pBranchLastNode : this.uCount= this.pFirstNode->BranchCount : this.bHashLen=Lcontext(by).bLcHashLen
    Return 1
End Property

Property List.Follow(pList As List) As Byte
    Dim As ListNode Ptr pTemp1=pList.Relation : bAlrdyTracked=0
    If pTrackTmp=pNode Then : pFirstNode=pFirstNodeTMP : pLastNode=pLastNodeTMP : this.uCount=pFirstNode->BranchCount : End If
    this.pFirstNode->BranchCount = this.uCount : this.pFirstNode->pBranchLastNode = this.pLastNode : bTracking=1
    If pTemp1<>0 And pTemp1->Tag0<>LIST_RES Then 
        pTrackTmp=pNode : this.pNode=pTemp1 : bTracking=1 
        pFirstNodeTMP=pFirstNode : pLastNodeTMP=pLastNode : pTrackPrevTmp=pTrackTmp->pPrev
        If pFirstNode->Tag0<>LIST_RES Then : bAlrdyTracked=0 : this.TrackCompute
        ElseIf pNode->pNext=pTrackTmp Or pNode=pTrackPrevTmp  Then : bAlrdyTracked=1: End If  'Or pNode->pPrev=pTrackTmp
        this.TrackSecure
        Return 1
    End If : Return 0 ' bAlrdyTracked=0 : this.TrackCompute : bTracking=0 : 
End Property

'==========================================================================================MEMORY MANAGEMENT
Property List.FlatCount As uInteger : Return this.pFlatRoot->pBranch->BranchCount : End Property
Property List.GarbageCount As uInteger : Return this.uGarbCt : End Property
Property List.ContainerCount As uInteger : Return this.uContainerGarbCt : End Property
Property List.NodeCount As uInteger : Return this.uNodeCOUNT : End Property

Property List.TagLenRedim(ub as uByte) as Byte
    If ub=1 Then : uB_RedimTagLen=ub : Return 1 : End If
    # IF TagMode=0 Or  TagMode=1
        uB_RedimTagLen=ub : Return 1
    # ELSE
        Print "Lzle warning : ListTagLenRedim(0) - No Redim - is only valid on # Define TagMode 0 and  # Define TagMode 1" : Return 0
    # ENDIF
End Property

Property List.TagLenDefault(ub as uByte) As Byte
    # IF TagMode=0
        uB_TagC_Len(uTag)=ub : Return 1
    # ELSE
        Print "Lzle warning : TagLenDefault (dynamic) is only valid on # Define TagMode=0" : Return 0
    # ENDIF
End Property

Property List.GarbageFlat As Byte
    Dim L_Context As ListContext : Dim As ListNode Ptr pTemp1, pTemp2, pTemp3 : Dim i as Byte
    If pFlatRoot->pBranch=0 Then : Return 0 : End If
    If pFlatRoot->pBranch->pNext=pFlatRoot->pBranch Then : pLocalMove=pFlatRoot->pBranch : pLocalMove->pBranch=0 : pLocalMove->Tag1=0 : pLocalMove->Tag0=LIST_DEL : this.NodeRecycle : Return 0 : End If
    If pFlatRoot->pBranch->pNext=pFlatRoot->pBranch->pBranchLastNode Then : Return 0 : End If
    pTemp1=pFlatRoot->pBranch->pNext : pTemp2=pEndFlat->pPrev
    If pTemp2=0 Then
        L_Context.pNode=pNode : L_Context.pFirstNode=pFirstNode : L_Context.pLastNode=pLastNode : L_Context.uCount=this.uCount
        this.FlatStack : pTemp2=pEndFlat->pPrev
        pNode=L_Context.pNode : pFirstNode=L_Context.pFirstNode : pLastNode=L_Context.pLastNode : this.uCount=L_Context.uCount
    End If :  If pTemp1=pTemp2 Then : Return 0 : End If
    pFlatRoot->pNext->pPrev=pTemp2 : pTemp2->pNext=pFlatRoot->pNext
    pFlatRoot->pNext=pTemp1 : pTemp1->pPrev=pFlatRoot
    Do
        pTemp1->Tag0=LIST_DEL : pTemp1->pBranchLastNode=0 : pTemp1->BranchCount=0
        pTemp3=pNode : pNode=pTemp1 : this.Val("") :
        If pTemp1->ListData<>0 Then           
            For i=MIN_COLS To MAX_COLS
                # IF TagMode=0
                    *pTemp1->ListData->str_tag_C(i)=""
                # ELSE
                    pTemp1->ListData->str_tag_C(i)=""
                # ENDIF
            Next i
            pTemp1->ListData->pNextContainer=pPanCakeGarbage->pNextContainer : pPanCakeGarbage->pNextContainer=pTemp1->ListData : pTemp1->ListData=0 : uContainerGarbCt+=1
        End If
        pNode=pTemp3
        pTemp1=pTemp1->pNext
    Loop Until pTemp1=pTemp2
    pTemp1->Tag0=LIST_DEL
    If pTemp1->ListData<>0 Then         
        For i=MIN_COLS to MAX_COLS
            # IF TagMode=0
                *pTemp1->ListData->str_tag_C(i)=""
            # ELSE
                pTemp1->ListData->str_tag_C(i)=""
            # ENDIF
        Next i 
    End If
    pTemp1->pBranchLastNode=0 : pTemp1->BranchCount=0
    pTemp3=pNode : pNode=pTemp1 : this.Val("") : pNode=pTemp3
    uGarbCt+=pFlatRoot->pBranch->BranchCount
    pFlatRoot->pBranch->pNext=pFlatRoot->pBranchLastNode : pFlatRoot->pBranchLastNode->pPrev=pFlatRoot->pBranch : pFlatRoot->pBranch->BranchCount=0
    If pFirstNode=pFlatRoot->pBranch Then : uCount=0 : this.RootPrivate : End If
    Return 1
End Property

Property List.Recycle As uInteger : Dim As ListNode Ptr pTemp=pNode : this.BlindTag(" ") : pNode=pTemp : this.Up : this.Root : this.AllOf :  Return this.AllRecycle : End Property  '

Property List.DropAll As uInteger  'pRoot principal + pFlatRoot + pFlatRoot->pBranch + pGarbage + pLastLAST/pWhyte = 5 nodes déalloues ds destructor     
    Dim As ListContainer Ptr P_tmp : Dim As uInteger i
    If this.IsDestroyed=1 Then : Return 0 : End If : this.NodeRecycle : this.NodeRecycle2
    Dim As ListNode Ptr pTemp=pNode : this.BlindTag(" ") : pNode=pTemp : this.Up : this.Root : this.GarbageFlat :  this.AllOf : this.AllRecycle : this.NodeRecycle : this.NodeRecycle2
    pTemp=pFlatRoot->pNext  : Dim iLong As uInteger=0   
    While pTemp<>pGarbage And pTemp<>0 And pTemp<>pFlatRoot
        If pFlatRoot->pNext->ListData<>0 Then
            If pFlatRoot->pNext->ListData->str_flat_tag<>0 Then : _Deallocate(pFlatRoot->pNext->ListData->str_flat_tag) : End If
            If pFlatRoot->pNext->ListData->str_item<>0 Then : _Deallocate(pFlatRoot->pNext->ListData->str_item) : End If
            # IF TagMode=0
                For i=MIN_COLS To MAX_COLS :  If pFlatRoot->pNext->ListData->str_tag_C(i)<>0 Then : _Deallocate(pFlatRoot->pNext->ListData->str_tag_C(i)) : End If  : Next i
        '    # ELSE
        '        If pFlatRoot->pNext->ListData->str_tag_C<>0 Then : _Deallocate(pFlatRoot->pNext->ListData->str_tag_C) : End If
            # ENDIF
            _Deallocate(pFlatRoot->pNext->ListData)
        End If
        _Deallocate(AllowCake) : pTemp=pFlatRoot->pNext : this.uNodeCOUNT-=1 : iLong+=1
    Wend   
    Dim pPanTemp As ListContainer Ptr =pPanCakeGarbage->pNextContainer : Dim SeekMt As Byte=this.bSeekMethod
    While pPanTemp<>pPanCakeGarbage
        P_tmp=AllowPanCake : If P_tmp->str_flat_tag<>0 Then : _Deallocate(P_tmp->str_flat_tag) : End If : If P_tmp->str_item<>0 Then : _Deallocate(P_tmp->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If P_tmp->str_tag_C(i)<>0 Then : _Deallocate(P_tmp->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate(P_tmp) : pPanTemp=pPanCakeGarbage->pNextContainer
    Wend
    this.bSeekMethod=SeekMt : uCount=0 : this.pFirstNode->BranchCount=0   
    If pFlatRoot->pBranch->pNext=pFlatRoot->pBranch Then :  _Deallocate(pFlatRoot->pBranch) : pFlatRoot->pBranch=0 : this.uNodeCOUNT-=1 : iLong+=1 : End If
    this.AllOfPrivate
    Return iLong   
End Property

Property List.Destroy As Byte
  '  If bPVSmethod<>-1 Then : Print "PVS_Count=" & PVS_Count  : End If '& " (the bigger, the most the optimization algo was used)" : End If
    Dim As uByte i
    If this.IsDestroyed=1 Then : Return 0 : End If
    this.Root : this.DropAll : IsDestroyed=1
    If pPanCakeGarbage<>0 Then
        If pPanCakeGarbage->str_flat_tag<>0 Then : _Deallocate(pPanCakeGarbage->str_flat_tag) : End If
        If pPanCakeGarbage->str_item<>0 Then : _Deallocate(pPanCakeGarbage->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pPanCakeGarbage->str_tag_C(i)<>0 Then : _Deallocate(pPanCakeGarbage->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate(pPanCakeGarbage) : pPanCakeGarbage=0
    End If
    If this.pFlatRoot<>0 Then
        If this.pFlatRoot->ListData<>0 Then : _Deallocate this.pFlatRoot->ListData : this.pFlatRoot->ListData=0 :  End If
        If this.pFlatRoot->pBranch<>0 Then
            If this.pFlatRoot->pBranch->pNext<>0 And pFlatRoot->pBranch->pNext<>pEndFlat Then               
                _Deallocate(this.pFlatRoot->pBranch->pNext) : pFlatRoot->pBranch->pNext=0 :   this.uNodeCOUNT-=1
            End If
            _Deallocate(this.pFlatRoot->pBranch) : pFlatRoot->pBranch=0 :  this.uNodeCOUNT-=1
        End If
        _Deallocate(this.pFlatRoot) : pFlatRoot=0 :  this.uNodeCOUNT-=1
    End If
    If this.pEndFlat->ListData<>0 Then
        If pEndFlat->ListData->str_flat_tag<>0 Then : _Deallocate(pEndFlat->ListData->str_flat_tag) : End If
        If pEndFlat->ListData->str_item<>0 Then : _Deallocate(pEndFlat->ListData->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pEndFlat->ListData->str_tag_C(i)<>0 Then : _Deallocate(pEndFlat->ListData->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate this.pEndFlat->ListData : this.pEndFlat->ListData=0     
    End If
    If this.pLastLASTNode->ListData<>0 Then
        If pLastLASTNode->ListData->str_flat_tag<>0 Then : _Deallocate(pLastLASTNode->ListData->str_flat_tag) : End If
        If pLastLASTNode->ListData->str_item<>0 Then : _Deallocate(pLastLASTNode->ListData->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pLastLASTNode->ListData->str_tag_C(i)<>0 Then : _Deallocate(pLastLASTNode->ListData->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate this.pLastLASTNode->ListData : this.pLastLASTNode->ListData=0
    End If
    If this.pGarbage->ListData<>0 Then
        If pGarbage->ListData->str_flat_tag<>0 Then : _Deallocate(pGarbage->ListData->str_flat_tag) : End If
        If pGarbage->ListData->str_item<>0 Then : _Deallocate(pGarbage->ListData->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pGarbage->ListData->str_tag_C(i)<>0 Then : _Deallocate(pGarbage->ListData->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate this.pGarbage->ListData : this.pGarbage->ListData=0
    End If
    If this.pWhyteMove->ListData<>0 Then
        If pWhyteMove->ListData->str_flat_tag<>0 Then : _Deallocate(pWhyteMove->ListData->str_flat_tag) : End If
        If pWhyteMove->ListData->str_item<>0 Then : _Deallocate(pWhyteMove->ListData->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pWhyteMove->ListData->str_tag_C(i)<>0 Then : _Deallocate(pWhyteMove->ListData->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate this.pWhyteMove->ListData : this.pWhyteMove->ListData=0
    End If
    If this.pFirstFIRSTNode->ListData<>0 Then
        If pFirstFIRSTNode->ListData->str_flat_tag<>0 Then : _Deallocate(pFirstFIRSTNode->ListData->str_flat_tag) : End If :
        If pFirstFIRSTNode->ListData->str_item<>0 Then : _Deallocate(pFirstFIRSTNode->ListData->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pFirstFIRSTNode->ListData->str_tag_C(i)<>0 Then : _Deallocate(pFirstFIRSTNode->ListData->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate this.pFirstFIRSTNode->ListData : this.pFirstFIRSTNode->ListData=0
    End If
    If this.pNode->ListData<>0 Then
        If pNode->ListData->str_flat_tag<>0 Then : _Deallocate(pNode->ListData->str_flat_tag) : End If : 
        If pNode->ListData->str_item<>0 Then : _Deallocate(pNode->ListData->str_item) : End If
        # IF TagMode=0
            For i=MIN_COLS To MAX_COLS :  If pNode->ListData->str_tag_C(i)<>0 Then : _Deallocate(pNode->ListData->str_tag_C(i)) : End If  : Next i
        # ENDIF
        _Deallocate this.pNode->ListData : this.pNode->ListData=0
    End If
    If this.pEndFlat<>0 Then : _Deallocate(this.pEndFlat) : pEndFlat=0 : This.uNodeCOUNT-=1 :  End If   
    If this.pLastLASTNode<>0 And pLastLASTNode<>pWhyteMove And pLastLASTNode<>pFirstFIRSTNode Then : _Deallocate(this.pLastLASTNode) : pLastLASTNode=0 : this.uNodeCOUNT-=1 :  End If
    If this.pGarbage<>0 And pGarbage<>pNode Then : _Deallocate(this.pGarbage) : pGarbage=0 : This.uNodeCOUNT-=1 :  End If
    If this.pWhyteMove<>0 Then : _Deallocate(this.pWhyteMove) : pWhyteMove=0 : This.uNodeCOUNT-=1 :  End If
    If this.pFirstFIRSTNode<>0 Then : _Deallocate(this.pFirstFIRSTNode) : pFirstFIRSTNode=0 :  this.uNodeCOUNT-=1 :  End If
    If this.pNode<>0 Then : _Deallocate(this.pNode) : pNode=0 : this.uNodeCOUNT-=1 :  End If
    If Listptemp<>0 Then : _Deallocate(Listptemp) : Listptemp=0 :  End If : If Listptemp2<>0 Then : _Deallocate(Listptemp2) : Listptemp2=0 :  End If : If zp3<>0 Then :  _Deallocate(zp3) : zp3=0 :  End If
    Return 0
End Property

'==========================================================================================DATA EXCHANGE
Property List.SnatchBelow(pList As List) As Byte
  Dim As ListNode Ptr pTemp1, pTemp2
    If pNode->Tag0=LIST_RES Or pNode->Tag0=LIST_DEL Or pNode=pWhyteMove Or pNode=pFlatRoot Or pNode=pLocalMove Or pNode=pLocalRoot Or pNode=pEndFlat Then : Return 0 : End If
    this.NodeRecycle : this.NodeRecycle2
    If bSnatchBLMethod=0 Then       
        If pNode->pBranch=0 Then
            this.Branch : this.BlindTag("") : pTemp1=pNode : pLocalMove=pNode : pLocalMove->pBranch=0 : pLocalMove->Tag1=0 : pLocalMove->Tag0=LIST_DEL
            If this.Snatch(pList) <> 1 Then : this.NodeRecycle : pLocalMove=pFirstNode : pFirstNode->pBranch->pBranch=0 : pFirstNode->pBranch=0 :  pLocalMove->Tag1=0 : pLocalMove->Tag0=LIST_DEL : this.NodeRecycle : Return 0
            Else : pTemp1->pPrev->pNext=pTemp1->pNext : pTemp1->pNext->pPrev=pTemp1->pPrev                 
            End If : this.uCount-=1 : pFirstNode->BranchCount-=1 : this.NodeRecycle
        Else : this.Branch : pNode=pFirstNode->pNext : If this.Snatch(pList) <> 1 Then : Return 0 : End If
        End If
 '       pTemp1=pFirstNode ' MAJ des pointeur pFirstNode->pPrev
 '       While pTemp1->pNext<>0 And pTemp1<>pFirstNode->pBranchLastNode : pTemp1=pTemp1->pNext : If pTemp1->pBranch<>0 Then : pTemp1->pBranch->pPrev=pFirstNode : End If : Wend
    Else       
        If pNode->pBranch=0 Then
            pTemp2=pList.GiveBranchDown : If pTemp2=0 Then : Return 0 : End If
            pTemp2->pBranch=pNode : pTemp2->pPrev=pFirstNode
            pNode->pBranch=pTemp2
            pLastNode=pTemp2->pBranchLastNode : pFirstNode=pTemp2 : pNode=pFirstNode->pNext : Return 1
        Else
            pTemp2=pList.GiveBranchDown : If pTemp2=0 Then : Return 0 : End If
            this.Branch :
            pTemp2->pPrev=pFirstNode->pPrev : pTemp2->pBranch=pFirstNode->pBranch : pTemp2->pBranch->pBranch=pTemp2
            pFirstNode->pNext->pPrev=pTemp2->pBranchLastNode : pTemp2->pBranchLastNode->pNext=pFirstNode->pNext
            pTemp2->pBranchLastNode=pFirstNode->pBranchLastNode
            pTemp1=pFirstNode ' MAJ des pointeur pFirstNode->pPrev
            While pTemp1->pNext<>0 And pTemp1<>pFirstNode->pBranchLastNode : pTemp1=pTemp1->pNext : If pTemp1->pBranch<>0 Then : pTemp1->pBranch->pPrev=pTemp2 : End If : Wend         
            pLocalMove=pFirstNode : pLocalMove->pBranch=0 : pLocalMove->Tag1=0 : pLocalMove->Tag0=LIST_DEL : pFirstNode=pTemp2
        End If
    End If
    this.UpLevel : Return 1
End Property

Property List.Snatch(pList As List) As Byte
    Dim As ListNode Ptr pTemp1, pTemp2
    this.NodeRecycle : this.NodeRecycle2
    If pNode->Tag0=LIST_DEL Then : If pFirstNode->pBranch=0 Then : pNode=pGarbage->pNext : Else : Return -1 : End If
    ElseIf pNode->Tag0=LIST_RES Then : pNode=pNode->pNext : Return 0 : End If ' If pNode=pWhyteMove Then : Return 0 : End If
    pTemp1=pList.GiveBranch : If pTemp1=0 Then : Return 0 : End If
    If bBranchCountDown=1 Then : this.BCountDown(pTemp1->BranchCount) : End If : uCount+=1 : pFirstNode->BranchCount+=1
    If pNode<>pLastNode Then : pNode->pNext->pPrev=pTemp1 : Else : pLastNode=pTemp1 : pFirstNode->pBranchLastNode=pTemp1 : End If
    If pTemp1->pBranch<>0 Then : pTemp1->pBranch->pPrev=pFirstNode  : End If
    pTemp1->pNext=pNode->pNext : pNode->pNext=pTemp1 : pTemp1->pPrev=pNode
    If pNode->pBranch<>0 Then : pNode->pBranch->pBranch=pNode : End If
    pList.AllOfPrivate : pNode=pTemp1 : this.VectorClear : Return 1
End Property

Property List.FlatSnatch(pList As List) As Byte   
    Dim pTemp1 As ListNode Ptr : Dim pTemp2 As ListNode Ptr
    pTemp1=pList.GiveFlat : If pTemp1=0 Then : Return 0 : End If   
    pFlatRoot->pBranch->BranchCount+=pTemp1->BranchCount : uNodeCOUNT+=pTemp1->BranchCount : pTemp1->BranchCount=0
    pTemp2=pTemp1->pBranch : pTemp1->pBranch=0 : pFlatRoot->pBranch->pNext->pPrev=pTemp2 : pTemp2->pNext=pFlatRoot->pBranch->pNext
    pFlatRoot->pBranch->pNext=pTemp1 : pTemp1->pPrev=pFlatRoot->pBranch : Return 1
End Property

Property List.GarbageSnatch(pList As List) As Byte   
    Dim As ListContainer Ptr pPanTemp1, pPanTemp2 : Dim As ListNode Ptr pTemp1, pTemp2
    pTemp1=pList.GiveGarbage : If pTemp1=0 Then : Return 0 : End If : uNodeCOUNT+=pTemp1->BranchCount : uGarbCt+=pTemp1->BranchCount
    pTemp2=pTemp1->pBranch : pFlatRoot->pNext->pPrev=pTemp2 : pTemp2->pNext=pFlatRoot->pNext : pTemp1->pBranch=0 : pFlatRoot->pNext=pTemp1 : pTemp1->pPrev=pFlatRoot   
    pPanTemp1=pPanCakeGarbage->pNextContainer
    pPanTemp2=pList.GivePanCake : If pPanTemp2=0 Then : Return 0 : End If :  pPanCakeGarbage->pNextContainer=pPanTemp2
    pPanTemp2=pList.GiveLastPanCake : uContainerGarbCt+=pList.GivePanCakeCount : pPanTemp2->pNextContainer=pPanTemp1 : Return 1
End Property

Property List.CopyCat(pList As List) As Byte
    Dim As ListNode Ptr pTmp1, pTmp2=pNode : Dim as byte by=1 : this.NodeRecycle
    pTmp1=pNode->pNext : pList.HashKeyUnique(0) : pList.HashSort(1) : pList.fSortMethod(1)
    this.pFirstNode->BranchCount = this.uCount : this.pFirstNode->pBranchLastNode = this.pLastNode
    pList.SetRelation(bCopyCatMethod) : bTracking=0 : bAlrdyTracked=1
    If this.uTag=0 Then
        If this.bCopyCatMethod=1 Then : While pNode<>pTmp1 and by=1 : pList.HashTag(this.HashTag) : pList.Check(this.Check) :: pList.SetRelation1(this.pNode) : by=this.HashStep : Wend
        Else : pList.BranchCountDown(0) : While pNode<>pTmp1 and by=1 : pList.HashTag(this.HashTag) : pList.Check(this.Check) : : pList.SetRelation2(this.pNode) : by=this.HashStep : Wend   
        End If
    Else
        If this.bCopyCatMethod=1 Then : While pNode<>pTmp1 and by=1 : pList.HashTag(this.Tag(uTag)) : pList.SetRelation1(this.pNode) : by=this.HashStep : Wend
        Else : pList.BranchCountDown(0) : While pNode<>pTmp1 and by=1 : pList.HashTag(this.Tag(uTag)) : pList.SetRelation2(this.pNode) : by=this.HashStep : Wend ' ?" #1 " & this.Tag(uTag) : 
        End If
    End If 
    pNode=pTmp2 : pList.HashKeyUnique(1) : Return 1
End Property

Property List.CopyTrack(pList As List) As Byte
    Dim as byte by=1 : Dim As ListNode Ptr pTmp1, pFirstTMP, pLastTMP
    If bTracking=0 Then : ? "LZLE warning : can't CopyTrack Track cession not specified" : Return 0 : End If'  bAlrdyTracked=0 : this.TrackSecure : 
    pFirstTMP=pFirstNode : pLastTMP=pLastNode : this.NodeRecycle
    pTmp1=pNode->pNext : pList.HashKeyUnique(0) : pList.HashSort(1) : pList.fSortMethod(1)
    this.pFirstNode->BranchCount = this.uCount : this.pFirstNode->pBranchLastNode = this.pLastNode
    pList.SetRelation(bCopyCatMethod)   
    If this.uTag=0 Then
        If this.bCopyCatMethod=1 Then : While pNode<>pTmp1 and by=1 : pList.HashTag(this.HashTag) : pList.Check(this.Check) : pList.SetRelation1(this.pNode) : by=this.TrackStep : Wend
        Else : pList.BranchCountDown(0) : While pNode<>pTmp1 and by=1 : pList.HashTag(this.HashTag) : pList.Check(this.Check)  : pList.SetRelation2(this.pNode) : by=this.TrackStep : Wend   
        End If
    Else
        If this.bCopyCatMethod=1 Then : While pNode<>pTmp1 and by=1 : pList.HashTag(this.Tag(uTag)) : pList.SetRelation1(this.pNode) : by=this.TrackStep : Wend
        Else : pList.BranchCountDown(0) : While pNode<>pTmp1 and by=1 : pList.HashTag(this.Tag(uTag)) : pList.SetRelation2(this.pNode) : by=this.TrackStep : Wend
        End If
    End If '  If  pList.Tracks(1).pNode=0 Then : ? "$List2 (1)= 0 " : Else : ? "$List2 (1) <> 0 " : End If
    pFirstNode=pFirstTMP : pLastNode=pLastTMP : this.uCount=pFirstNode->BranchCount : pList.HashKeyUnique(1) : Return 1
End Property
#endif

end 
