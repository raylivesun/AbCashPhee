declare function polymorph(byref morph as string, byref style as string, _
byref category as string, byval mo as string) as string

type morph
     style as string
     category as string
     mo as string
end type


#if 0
'==========================================================================================TYPE LIST PUBLIC PROPERTIES - HASH CONTROL
Property List.Root As Byte
    Dim pTemp As ListNode Ptr : Dim pTemp2 As ListNode Ptr : Dim pContextRetour As ListContext
    If pFirstNode->Tag0<>LIST_RES Then : bAlrdyTracked=0 : End If 
    If bTracking=1 And pFirstNode->Tag0<>LIST_RES Then : bAlrdyTracked=0 : this.TrackSecure : End If :  bTracking=0 
    If this.IsDestroyed=1 Then : Print "LZLE error - List.Root : Error List destroyed : instance can't be re-used" : Sleep : Return 0 : End If
    this.RootPrivate
    If pLocalMove<>0 Then
        If pLocalMove->ListData<>0 Then
            # IF TagMode=0
            *pLocalMove->ListData->str_tag_C(1)=""
            # ELSE
            pLocalMove->ListData->str_tag_C(1)=""
            # ENDIF
        End If
        pLocalMove->pBranch=0 :  pLocalMove->Tag0=LIST_DEL
    End If
    this.NodeRecycle : this.NodeRecycle2 : bSearchRes=0 : pTemp2=0 : this.bHashStepRev=0 : pLatestHTag=0
    'Changement de fonctionnement - Patch de compatibilité - : il faut un dernier node logique à blanc qui ne soit jamais 'flaté'
    If this.pWhyteMove<>0 Then
        pWhyteMove->Tag0=""
        If this.pWhyteMove->pNext<>0 And pWhyteMove->pNext->Tag0<>LIST_RES Then           
            If pWhyteMove->pPrev<>0 Then : this.pWhyteMove->pPrev->pNext=this.pWhyteMove->pNext : End If
            If pWhyteMove->pNext<>0 Then :this.pWhyteMove->pNext->pPrev=this.pWhyteMove->pPrev : End If         
        End If       
    End If   
    If pGarbage=0 Then  ' This.Tag(LIST_DEL) :
        This.BlindTag(LIST_DEL) : pGarbage=this.pNode : pGarbage->pPrev=pFlatRoot : this.Val(LIST_DEL) :
        If pFlatRoot->pNext<>pGarbage Then : pGarbage->pNext=pFlatRoot->pNext : End If : pFlatRoot->pNext=pGarbage
        If pGarbage->pNext<>0 Then : pGarbage->pNext->pPrev=pGarbage : End If : this.pNode = pGarbage
    End If
    If pFirstNode->pPrev=0 And pNode->pNext=0 Then : pTemp=pNode : this.AllOf : pNode=pTemp : End If
    'Gestion du contexte de la Flat List qui doit contenir un dernier node à blanc
    this.FlatStack(0)
    'Corrections  - Patch de compatibilité - : pFlatRoot se balade, il faut le remettre au début -
    If pFlatRoot->pPrev<>0 Then : pFlatRoot->pPrev->pNext=pFlatRoot->pNext : End If : If pFlatRoot->pNext<>0 Then :  pFlatRoot->pNext->pPrev=pFlatRoot->pPrev : End If
    pFlatRoot->pPrev=this.pFirstFIRSTNode : pFlatRoot->pNext=this.pFirstFIRSTNode->pNext : If this.pFirstFIRSTNode->pNext<>0 Then : this.pFirstFIRSTNode->pNext->pPrev=pFlatRoot : End If : this.pFirstFIRSTNode->pNext=pFlatRoot
    'Changement de fonctionnement - Patch de compatibilité - pLastLAST devient dernier node LOGIQUE : on le remet à jour
    If this.pFirstFIRSTNode->pBranchLastNode<>0 Then
        pTemp=this.pFirstFIRSTNode->pBranchLastNode : While pTemp->pBranchLastNode<>0 And pTemp<>pTemp2 : pTemp2=pTemp : pTemp=pTemp->pBranchLastNode : Wend : this.pLastLASTNode=pTemp       
    End If     
    'NodeFlat+Restorehash nécessite la présence d'un dernier node fictif
    If this.pLastNode->Tag0<>"" Then : If pWhyteMove<>0 Then : this.AllOf  : Else : pTemp=this.pNode : this.pFirstNode->BranchCount=this.uCount : this.BlindTag("") : this.pWhyteMove=this.pNode : this.pNode=pTemp : End If : End If
    this.pNode=pGarbage : this.uCount=this.pFirstFIRSTNode->BranchCount : this.pLastLASTNode->pPrev->pNext=this.pLastLASTNode
    If this.pLastLASTNode->pNext->Tag0=LIST_RES Then : this.pLastLASTNode->pNext=0 : End If
  ' Option for .Root become compatible with Rev parse with no need to jump to Last node (List.Last)
    this.NodeRecycle : this.pNode=AllowCake : pNode->Tag0="" : pLocalMove=pNode : pNode->pNext=pGarbage->pNext
    # IF TagMode=0
        If pLocalMove->ListData<>0 Then : *pLocalMove->ListData->str_tag_C(1)="" : End If
    # ELSE
        If pLocalMove->ListData<>0 Then : pLocalMove->ListData->str_tag_C(1)="" : End If
    # ENDIF
    pLocalMove->pBranch=0 :  pLocalMove->Tag0=LIST_DEL
    If pLastNode=pWhyteMove Then : pNode->pPrev=pLastNode->pPrev : Else : pNode->pPrev=pLastNode : End If
    this.NodeRecycle2 : this.pFirstNode->BranchCount=this.uCount : this.pFirstNode->pBranchLastNode=this.pLastNode
    uB_CurLevel=1 : uB_Level=1 : If bnStepByPass=0 Then :  uB_BottomLevel=255 : uB_MaxLevel=1 : uB_KeyStepCumul=1 : End If : bfStepZero=0
    Return 1
End Property

Property List.FlatStack As Byte : this.FlatStack(1) : this.AllOf : bSearchRes=0 : Return 1 : End Property
Property List.RootNode As Byte : bSearchRes=0 : this.Root : this.pNode=This.pFirstFIRSTNode : Return 1 : End Property
Property List.EndNode As Byte : bSearchRes=0 : this.Root : this.pNode=This.pLastLASTNode : Return 1 : End Property

Property List.HashStep As Byte
    While this.pnode->pBranch<>0
        this.pFirstNode=this.pNode->pBranch : this.uCount=this.pFirstNode->BranchCount : this.pLastNode=this.pNode->pBranch->pBranchLastNode : this.pNode=this.pNode->pBranch
        If pnode<>pLastNode Then : pnode=pnode->pNext : If pNode=pWhyteMove Then : this.AllOf : Return 0 : Else : Return 1 : End If : End If ' If pNode=pWhyteMove Then : Return 0 : Else : Return 1 : End If
    Wend : If pnode<>pLastNode Then : pnode=pnode->pNext :  If pNode=pWhyteMove Then : this.AllOf : Return 0 : Else : Return 1 : End If : End If
    While pFirstNode->pBranch<>0
        pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : uCount=pFirstNode->BranchCount
        pLastNode=pFirstNode->pBranchLastNode : If pnode<>pLastNode Then : pnode=pnode->pNext :  If pNode=pWhyteMove Then : this.AllOf : Return 0 : Else : Return 1 : End If : End If
    Wend : this.RootPrivate : Return 0
End Property

Property List.HashStepRev As Byte
    this.bHashStepRev=1
    While this.pnode->pBranch<>0
        this.pFirstNode=this.pNode->pBranch : this.uCount=this.pFirstNode->BranchCount : this.pLastNode=this.pNode->pBranch->pBranchLastNode : this.pNode=this.pNode->pBranch
        If pLastNode=pWhyteMove And pLastNode->pPrev<>0 Then : pNode=pLastNode->pPrev : Else : pnode=pLastNode : End If : Return 1
    Wend : If pnode->pPrev=pGarbage Then : bHashStepRev=0 : Return 0 : End If : If pnode->pPrev<>pFirstNode Then : this.pnode = this.pnode->pPrev : Return 1 : End If
    While pFirstNode->pBranch<>0
        pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : uCount=pFirstNode->BranchCount
        pLastNode=pFirstNode->pBranchLastNode : If this.pnode <> this.pFirstNode->pNext Then : this.pnode = this.pnode->pPrev : If pnode=pGarbage Then : bHashStepRev=0 : Return 0 : End If : Return 1 : End If
    Wend : this.bHashStepRev=0 : Return 0
End Property

Property List.KeyStep As Byte : While this.HashStep=1 : If pNode->Tag1<>0 And pNode->Tag1<>255 Then : Return 1 : End If : Wend : End Property
Property List.KeyStepRev As Byte : While this.HashStepRev=1 : If pNode->Tag1<>0 And pNode->Tag1<>255 Then : Return 1 : End If : Wend : End Property
Property List.KeyStep(ub as uByte) As Byte : While this.HashStep=1 : If pNode->Tag1=ub Then : Return 1 : End If : Wend : End Property
Property List.KeyStepRev(ub as uByte) As Byte : While this.HashStepRev=1 : If pNode->Tag1=ub Then : Return 1 : End If : Wend : End Property

'Numeric parse optimization
Property List.nCurLevel As Byte : Return uB_CurLevel  : End Property
Property List.nCurLevel(uB_len as uByte) As Byte : If uB_len=uB_CurLevel Then : Return 1 : Else : Return 0 : End If : End Property

'Numeric sorted parse
Property List.nHashStep(uB_len as uByte) As Byte   
    While this.pnode->pBranch<>0 And uB_CurLevel<=uB_len
        uB_CurLevel +=1 : If uB_CurLevel>uB_MaxLevel Then : uB_MaxLevel=uB_CurLevel : End If
        this.pFirstNode=this.pNode->pBranch : this.uCount=this.pFirstNode->BranchCount : this.pLastNode=this.pNode->pBranch->pBranchLastNode : this.pNode=this.pNode->pBranch
        If pnode<>pLastNode Then : pnode=pnode->pNext : If pNode=pWhyteMove Then : this.AllOf : Return 0 : Else : Return 1 : End If : End If
    Wend :     
    If pnode<>pLastNode Then : pnode=pnode->pNext :  If pNode=pWhyteMove Then : this.AllOf : Return 0 : Else : Return 1 : End If : End If   
    While pFirstNode->pBranch<>0
        uB_CurLevel -=1
        pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : uCount=pFirstNode->BranchCount
        pLastNode=pFirstNode->pBranchLastNode : If pnode<>pLastNode Then : pnode=pnode->pNext :  If pNode=pWhyteMove Then : this.AllOf : uB_Level+=1 : Return 0 : Else : Return 1 : End If : End If
    Wend :
    this.RootPrivate : Return 0
End Property
Property List.nKeyStep(uB_len as uByte) As Byte : While this.nHashStep(uB_len)=1 : If pNode->Tag1<>0 And pNode->Tag1<>255 Then : Return 1 : End If : Wend : Return 0 : End Property
Property List.nKeyStep as Byte
    While uB_KeyStepCumul<=uB_MaxLevel
        While this.nKeyStep(uB_Level) : If this.nCurLevel(uB_Level)=1 Then : Return 1 : End If : Wend
        bnStepByPass=1 : this.Root : bnStepByPass=0 : uB_Level+=uB_KeyStepCumul : uB_KeyStepCumul+=1
    Wend : uB_CurLevel=1 : uB_Level=1 : uB_MaxLevel=1 : uB_KeyStepCumul=1 : Return 0
End Property

'Numeric sorted Reverse parse
Property List.nHashStepRev(uB_len as uByte) As Byte   
    this.bHashStepRev=1
    While this.pnode->pBranch<>0 And uB_CurLevel<=uB_len
        uB_CurLevel +=1 : If uB_CurLevel>uB_MaxLevel Then : uB_MaxLevel=uB_CurLevel : uB_BottomLevel = uB_MaxLevel+1 : End If
        this.pFirstNode=this.pNode->pBranch : this.uCount=this.pFirstNode->BranchCount : this.pLastNode=this.pNode->pBranch->pBranchLastNode : this.pNode=this.pNode->pBranch
        If pLastNode=pWhyteMove And pLastNode->pPrev<>0 Then : pNode=pLastNode->pPrev : Else : pnode=pLastNode : End If : Return 1
    Wend : If pnode->pPrev=pGarbage Then : Return 0 : End If : If pnode->pPrev<>pFirstNode Then : this.pnode = this.pnode->pPrev : Return 1 : End If
    While pFirstNode->pBranch<>0
        uB_CurLevel -=1
        pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : uCount=pFirstNode->BranchCount
        pLastNode=pFirstNode->pBranchLastNode : If this.pnode <> this.pFirstNode->pNext Then : this.pnode = this.pnode->pPrev : If pnode=pGarbage Then : Return 0 : End If : Return 1 : End If
    Wend : Return 0
End Property
Property List.nKeyStepRev(uB_len as uByte) As Byte : While this.nHashStepRev(uB_len)=1 : If pNode->Tag1<>0 And pNode->Tag1<>255 Then : Return 1 : End If : Wend : Return 0 : End Property
Property List.nKeyStepRev as Byte
   Dim t as Integer
   For t=uB_BottomLevel to 1 Step-1
        While this.nKeyStepRev(uB_BottomLevel) : If this.nCurLevel(uB_BottomLevel)=1 Then : Return 1 : End If : Wend : bnStepByPass=1 : this.Root : bnStepByPass=0 : uB_BottomLevel-=uB_KeyStepCumul
    Next t : uB_CurLevel=1 : uB_BottomLevel=255 : uB_KeyStepCumul=1 : Return 0
End Property

Property List.HashTag(str_Tag As String) As Byte
    Dim As ListNode Ptr pTemp,  pTemp02, pTemp03 , pTemp04
    Dim As uInteger i=0, iLen=Len(str_tag), iLenStrTmp, iLenpNode, iLenCumul, iLenDiff, iLenDelta=0, PvsiStep
    Dim As uByte HadHashTag=1, istep=this.bHashLen, IsLast=0, IsPt2Swp=0, IsPtSwp=0, bSeekMethod_TMP=bSeekMethod, bAutoCursor_TMP=bAutoCursor
    Dim As zString Ptr zp1, zp2, Listptemp2_b, Listptemp_b : Dim As String str_testTMP
    If iLen>DEF_KEYLEN Then : Listptemp2_b=_CAllocate(iLen+1) : Swap Listptemp2,Listptemp2_b : IsPt2Swp=1 : End If
    str_testTMP=str_Tag  ': If bNFTrackedOut=1 Then : bAlrdyTracked=0 : This.TrackCompute : bNFTrackedOut=0 : End If
    If pFirstNode<>pFirstFIRSTNode Then 'Déclenchement optimisation // Si pFirstNode=pFirstFIRSTNode il est possible que la liste soit vide ou que le node en cours soit en cours d'envoi au garbage collector
        If pLatestHTag=pNode Then : Str_tmp=sLatestHTag '  Algo optimisation 1 : basé sur la récupération la + rapide possible de la position dans l'arbre // And bPVSmethod=0 //  l'algo d'optimisation le plus simple est souvent le + efficace et reste prioritaire sur PVS
        Else : pNode=pFirstNode->pNext : Str_tmp=this.HashTag ' If pFirstNode->Tag1<>"" Then : Str_tmp=pFirstNode->Tag1 & pNode->Tag0 : Else : Str_tmp=this.HashTag  : End If ': If ubKeysRegister=1 Then : pFirstNode->Tag1=Left(Str_tmp, Len(Str_tmp)-istep) : End If
        End If
        iLenStrTmp=Len(Str_tmp)
        If iLenStrTmp>DEF_KEYLEN Then : Listptemp_b=_CAllocate(iLenStrTmp+1) : Swap Listptemp,Listptemp_b : IsPtSwp=1 : End If
        If iLen>iLenStrTmp Then : iLenDiff=iLen-iLenStrTmp : End If
       
        *Listptemp=Str_tmp : zp1=Listptemp : zp1+=istep : (*zp1)[0]=0 ' *Alternative to *Listptemp=Left(Str_tmp,istep)
        *Listptemp2=str_tag : zp2=Listptemp2 : zp2+=istep : (*zp2)[0]=0 ' *Alternative to *Listptemp2=Left(str_tag,istep)
        If *Listptemp=*Listptemp2  And bPVSmethod<1 Then  ' Algo optimisation 1 & 2 : on cale la longueur de la clef predictive sur celle de la clef à atteindre ou à créer, en remontant dans l'arbre le cas échéant (en utilisant la fonction d'upwelling de l'arbre : pointeurs directs) // And bPVSmethod=0
            pFirstNode->BranchCount=uCount :  pFirstNode->pBranchLastNode=pLastNode
            If MAX_HASHLEN=1 Then : iLenpNode=1 : Else : iLenpNode=Len(pNode->Tag0) : End If
            While iLen<iLenStrTmp : pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : iLenStrTmp=Len(Str_tmp)-iLenpNode :  Str_tmp=Left(Str_tmp, iLenStrTmp) : If MAX_HASHLEN=1 Then : iLenpNode=1 : Else : iLenpNode=Len(pNode->Tag0) : End If :  Wend   
            pLastNode=pFirstNode->pBranchLastNode : uCount=pFirstNode->BranchCount
            iLenStrTmp=Len(Str_tmp) ': While iLen<Len(Str_tmp) : this.UpLevel : Str_tmp=this.HashTag :  Wend  '  pNode=pFirstNode->pNext : iLenpNode=Len(pNode->Tag(uTag)) :
            pFirstNode->BranchCount=uCount :  pFirstNode->pBranchLastNode=pLastNode
            While Left(str_tag, iLenStrTmp)<>Str_tmp  ' Algo optimisation 1 & 2 : on redescendra ensuite dans l'arbre depuis le point de l'arbre le plus bas commun aux deux clefs = économie de lookup si la prédiction est réalisée
                Str_tmp=Left(Str_tmp, iLenStrTmp-iLenpNode)
                iLenStrTmp-=iLenpNode : iLenCumul +=iLenpNode : pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : iLenpNode=istep
            Wend : pLastNode=pFirstNode->pBranchLastNode : uCount=pFirstNode->BranchCount
            iLen=iLenCumul+istep+iLenDiff : str_Tag=Right(str_Tag, iLen) : pTemp02=pFirstNode : iLenDelta=iLenStrTmp-iLenpNode
            If pFirstNode=pFirstFIRSTNode Then : pTemp02=pGarbage : End If ' And pLatestHTag=pNode             
           
        ElseIf bPVSmethod>-1 Then ' Algo optimisation 2 : basé sur l'enregistrement à un niveau PvsiStep càd (lngClef/n)+1 de la longueur de la clef, d'un pointeur "prédictif" vers une branche plus basse de l'arbre
            PvsiStep=iLen/PVS_ratio+1
            *Listptemp=Str_tmp : zp1=Listptemp : zp1+=PvsiStep : (*zp1)[0]=0 ' *Alternative to *Listptemp=Left(Str_tmp,istep)
            *Listptemp2=str_tag : zp2=Listptemp2 : zp2+=PvsiStep : (*zp2)[0]=0 ' *Alternative to *Listptemp2=Left(str_tag,istep)       
            If  *Listptemp<>*Listptemp2 Then' *Alternative to If Left(Str_tmp,istep)<>Left(str_tag,istep) Then   *Listptemp<>*Listptemp2     
                pFirstNode->BranchCount=uCount : pFirstNode->pBranchLastNode=pLastNode : pFirstNode=pFirstFIRSTNode
                this.uCount = this.pFirstNode->BranchCount : pLastNode=this.pFirstNode->pBranchLastNode : pTemp02=pGarbage
                this.HashTag(*Listptemp2 ) 'youpi
                If pNode->BranchCount<>0 Then : pFirstNode=Cast(ListNode Ptr, pNode->BranchCount) : pLastNode=pFirstNode->pBranchLastNode : End If :  pTemp04=pNode : PVS_Count+=1               
                pNode=pFirstNode->pNext : pNode=pFirstNode->pNext
                *Listptemp=Str_tmp : zp1=Listptemp : zp1+=istep : (*zp1)[0]=0 ' *Alternative to *Listptemp=Left(Str_tmp,istep)
            End If
            If *Listptemp=*Listptemp2 Then  ' Algo optimisation 1 & 2 : on cale la longueur de la clef predictive sur celle de la clef à atteindre ou à créer, en remontant dans l'arbre le cas échéant (en utilisant la fonction d'upwelling de l'arbre : pointeurs directs)
                pFirstNode->BranchCount=uCount :  pFirstNode->pBranchLastNode=pLastNode
                If MAX_HASHLEN=1 Then : iLenpNode=1 : Else : iLenpNode=Len(pNode->Tag0) : End If
                While iLen<iLenStrTmp : pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : iLenStrTmp=Len(Str_tmp)-iLenpNode :  Str_tmp=Left(Str_tmp, iLenStrTmp) : If MAX_HASHLEN=1 Then : iLenpNode=1 : Else : iLenpNode=Len(pNode->Tag0) : End If :  Wend   
                pLastNode=pFirstNode->pBranchLastNode : uCount=pFirstNode->BranchCount
                iLenStrTmp=Len(Str_tmp) ' iLenpNode=Len(pNode->Tag(uTag)) :
                pFirstNode->BranchCount=uCount :  pFirstNode->pBranchLastNode=pLastNode
                While Left(str_tag, iLenStrTmp)<>Str_tmp  ' Algo optimisation 1 & 2 : on redescendra ensuite dans l'arbre depuis le point de l'arbre le plus bas commun aux deux clefs = économie de lookup si la prédiction est réalisée
                    Str_tmp=Left(Str_tmp, iLenStrTmp-iLenpNode)
                    iLenStrTmp-=iLenpNode : iLenCumul +=iLenpNode : pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : iLenpNode=istep
                Wend : pLastNode=pFirstNode->pBranchLastNode : uCount=pFirstNode->BranchCount
                iLen=iLenCumul+istep+iLenDiff : str_Tag=Right(str_Tag, iLen) : pTemp02=pFirstNode : iLenDelta=iLenStrTmp-iLenpNode
                If pFirstNode=pFirstFIRSTNode Then : pTemp02=pGarbage : End If ' And pLatestHTag=pNode   
            Else               
                pFirstNode->BranchCount=uCount : pFirstNode->pBranchLastNode=pLastNode : pFirstNode=pFirstFIRSTNode
                this.uCount = this.pFirstNode->BranchCount : pLastNode=this.pFirstNode->pBranchLastNode : pTemp02=pGarbage : bSeekMethod_TMP=1               
            End If
        Else
            pFirstNode->BranchCount=uCount : pFirstNode->pBranchLastNode=pLastNode : pFirstNode=pFirstFIRSTNode
            this.uCount = this.pFirstNode->BranchCount : pLastNode=this.pFirstNode->pBranchLastNode : pTemp02=pGarbage : bSeekMethod_TMP=1
        End If
    Else
        pTemp02=pGarbage : bSeekMethod_TMP=1
    End If ' Fin algo optimisation   
    iLenCumul=0 : *Listptemp2=str_Tag : zp1=Listptemp2 ' * Alternative to Mid
    For i=1 to Len(str_Tag) step istep
        zp2=zp1 : zp1+=istep : (*zp3)[0]=(*zp1)[0] : (*zp1)[0]=0 : Str_tmp=*zp2 : (*zp1)[0]=(*zp3)[0] ' * Alternative to Mid :  Str_tmp=Mid(str_Tag,i, istep)
        iLenCumul+=iStep
        If bHTmethod=0 Then
        If bSeekMethod_TMP=2 Then : pTemp = this.pLastNode : While ( pTemp- >Tag0<>Str_tmp  AndAlso pTemp<>pTemp02 ) : pTemp = pTemp->pPrev : Wend
        Else : pTemp = pTemp02 : While ( pTemp- >Tag0<>Str_tmp  AndAlso pTemp<>pLastNode ) : pTemp = pTemp->pNext : Wend
        End If
            
        Else : uB_tmp=0
            If bSeekMethod_TMP=2 Then
                pTemp = this.pLastNode : If pTemp=pWhyteMove Then : pTemp = pTemp->pPrev : End If
                If uSortTag=0 Then : While ( pTemp->Tag0>Str_tmp  And pTemp<>pTemp02 ) : pTemp = pTemp->pPrev : Wend
                Else  '  While ( pTemp->ListData->str_tag_C(uSortTag)>Str_tmp  And pTemp<>pTemp02 ) : pTemp = pTemp->pPrev : Wend
                    # IF TagMode=0
                        If pTemp->ListData<>0 Then : If *pTemp->ListData->str_tag_C(uSortTag)>Str_tmp  Then : uB_tmp=1 : End If : End If
                        While (uB_tmp=1 AndAlso pTemp<>pTemp02) : pTemp = pTemp->pPrev : If pTemp->ListData<>0 Then : If *pTemp->ListData->str_tag_C(uSortTag)>Str_tmp Then : uB_tmp=1 : End If : End If : Wend
                    # ELSE                   
                        If pTemp->ListData<>0 Then : If pTemp->ListData->str_tag_C(uSortTag)>Str_tmp  Then : uB_tmp=1 : End If : End If
                        While (uB_tmp=1 AndAlso pTemp<>pTemp02) : pTemp = pTemp->pPrev : If pTemp->ListData<>0 Then : If pTemp->ListData->str_tag_C(uSortTag)>Str_tmp Then : uB_tmp=1 : End If : End If : Wend
                    # ENDIF
                End If
                If pTemp=pLastNode Then : IsLast=1 : End If : If pTemp=pFirstNode Then : pTemp=pTemp->pNext : End If
            Else                 
                If uSortTag=0 Then : pTemp=pTemp02 : While (pTemp->Tag0<Str_tmp  AndAlso pTemp<>pLastNode) : pTemp=pTemp->pNext : Wend : If pTemp->Tag0<Str_tmp  AndAlso pTemp=pLastNode Then : IsLast=1 : End If ': Print "**1" : sleep
                Else  'While (pTemp->ListData->str_tag_C(uSortTag)<Str_tmp  AndAlso pTemp<>pLastNode) : pTemp=pTemp->pNext : Wend : If pTemp->ListData->str_tag_C(uSortTag)<Str_tmp  AndAlso pTemp=pLastNode Then : IsLast=1 : End If
                    pTemp=pTemp02
                    # IF TagMode=0
                        If pTemp->ListData<>0 Then : If *pTemp->ListData->str_tag_C(uSortTag)<Str_tmp  Then : uB_tmp=1 : End If : End If
                        While (uB_tmp=1 AndAlso pTemp<>pLastNode) : pTemp = pTemp->pNext : If pTemp->ListData<>0 Then : If *pTemp->ListData->str_tag_C(uSortTag)<Str_tmp Then : uB_tmp=1 : End If : End If : Wend
                        If *pTemp->ListData->str_tag_C(uSortTag)<Str_tmp  AndAlso pTemp=pLastNode Then : IsLast=1 : End If
                    # ELSE                   
                        If pTemp->ListData<>0 Then : If pTemp->ListData->str_tag_C(uSortTag)<Str_tmp  Then : uB_tmp=1 : End If : End If
                        While (uB_tmp=1 AndAlso pTemp<>pLastNode) : pTemp = pTemp->pNext : If pTemp->ListData<>0 Then : If pTemp->ListData->str_tag_C(uSortTag)<Str_tmp Then : uB_tmp=1 : End If : End If : Wend
                        If pTemp->ListData->str_tag_C(uSortTag)<Str_tmp  AndAlso pTemp=pLastNode Then : IsLast=1 : End If
                    # ENDIF
                End If
            End If
        End If
        Str_tmp2=pTemp- >Tag0  ':  Print "**2" : sleep If uTag=0..
        If Str_tmp2=Str_tmp And bHashKeyUnique=1 Then : this.pNode = pTemp
        ElseIf Str_tmp2=Str_tmp And bHashKeyUnique=0 And iLenCumul<iLen Then : this.pNode = pTemp
        ElseIf bHTmethod=1 And IsLast=0 Then           
            pTemp03=AllowCake : this.uCount+=1
            pTemp03->pNext=pTemp : pTemp03->pPrev=pTemp->pPrev : pTemp->pPrev->pNext=pTemp03 : pTemp->pPrev=pTemp03         
            If bBranchCountDown=1 Then : this.BCountDown(1) : End If
            pTemp03->Tag0 = Str_tmp  '  If uTag=0..
            HadHashTag=0 : pFirstNode->BranchCount+=1 : pNode = pTemp03
            If bTrackMultiKeys>0 And bCopyCatRelation=0 Then                 
                If Str_tmp2=Str_tmp Then : If pNode->pNext->pBranchLastNode=0 Then : pNode=pNode->pNext : this.HoldBack(uBTrackTarget) : pNode=pTemp03 : End If : this.HoldBack(uBTrackTarget) : End If
            End If
        Else
            pTemp03 = this.pLastNode : this.uCount+=1 : pTemp03->pNext = this.AllowCake 'And eat it
            pTemp03->pNext->pPrev = this.pLastNode
            pTemp03->pNext->Tag0 = Str_tmp  '  If uTag=0..
            this.pLastNode = pTemp03->pNext : this.pNode = pTemp03->pNext : If bBranchCountDown=1 Then : this.BCountDown(1) : End If
            pLastNode->pPrev = pTemp03 : HadHashTag=0 : pFirstNode->BranchCount+=1
            If bTrackMultiKeys>0 And bCopyCatRelation=0 Then                 
                If Str_tmp2=Str_tmp Then : this.HoldBack(uBTrackTarget) : End If
            End If
     '  Else : Print "LZLE error - attempt to clean process and aborting."  : Print this.DropAll & " / " & this.NodeCount : sleep : system               If bSeekMethod_TMP=2 Then : Print "Pt 9" : sleep :  End If
        End If
       
        If iLenCumul<iLen Then
            pFirstNode->BranchCount=this.uCount : pFirstNode->pBranchLastNode=pLastNode : pTemp=pNode
            If this.pNode->pBranch=0 Then ' New 'Hash' : this.BlindTag(LIST_RES) :
                pTemp03 = this.pLastNode : this.uCount+=1 : pTemp03->pNext = this.AllowCake 'And eat it
                pTemp03->pNext->pPrev = pTemp03 :  pTemp03->pNext->Tag0 = LIST_RES ' : Else : pTemp03->pNext->Tag1 = 3 : End If ' pTemp03->pNext- >Tag0 = LIST_RES
                pTemp03 = pTemp03->pNext : this.pLastNode = pTemp03 : pNode=pTemp03
                this.pNode->pPrev=this.pFirstNode : pNode->pBranch = pTemp : pTemp->pBranch=this.pNode
                this.uCount=0 : this.pFirstNode=pTemp->pBranch : this.pNode = this.pFirstNode
            Else 'Branche déjà créée
                this.pFirstNode = this.pNode->pBranch : this.uCount = this.pFirstNode->BranchCount
                this.pLastNode = this.pNode->pBranch->pBranchLastNode : this.pNode = this.pNode->pBranch
            End If
        End If : pTemp02=pFirstNode
    Next i
    this.pLatestHTag=this.pNode : this.sLatestHTag=str_testTMP : str_Tag=str_testTMP
    this.pFirstNode->pBranchLastNode = this.pLastNode
    If this.pNode->Tag1=0 Then :  If HadHashTag=1 Then : HadHashTag=2 : End If : If bRHByPass=0 Then : this.pNode->Tag1=1 : End If : End If
    If pTemp04<>0 Then : pTemp04->BranchCount=CuInt(pFirstNode)  : End If
    If IsPt2Swp=1 Then : Swap Listptemp2,Listptemp2_b : _Deallocate(Listptemp2_b) : End If : If IsPtSwp=1 Then : Swap Listptemp,Listptemp_b : _Deallocate(Listptemp_b) : End If     
    Return HadHashTag
End Property

Property List.HashTag As String
    Dim As ListNode Ptr pTemp01= this.pFirstNode, pTemp02=this.pNode    
    pTemp01 = this.pFirstNode : pTemp02 = this.pNode
    If pFirstNode->pBranch<>pFlatRoot Then : Str_tmp = this.pnode->Tag0 'si pas ds contexte flat root on prends la clef Tag0
    ElseIf bTracking=1 And this.pnode->ListData=0 Then : Str_tmp = this.pnode->Tag0 ': bAlrdyTracked=0 : This.TrackCompute : pTemp01 = this.pFirstNode : pTemp02 = this.pNode
    Else : Str_tmp = *this.pnode->ListData->str_flat_tag  : End If
    While pTemp01->pPrev<>0
        pTemp02 = pTemp01->pBranch
        Str_tmp = pTemp02->Tag0  + Str_tmp                 
        pTemp01 = pTemp01->pPrev
    Wend   
    Return Str_tmp
End Property

Property List.HasTagTree(str_Tag As String) As Byte
    Dim pTemp As ListNode Ptr
    this.sSearchTag = str_Tag   
    If this.bSeekMethod=1 Then
        pTemp = this.pFirstNode : If this.pGarbage<>0 And this.pFirstNode=this.pFirstFIRSTNode Then : pTemp = this.pGarbage  : End If
        While (pTemp->pNext <> 0 And pTemp->Tag0 <> str_Tag  AND pTemp <> this.pLastNode ) : pTemp = pTemp->pNext : Wend
    ElseIf this.bSeekMethod=2 Then
        pTemp = this.pLastNode
        While (pTemp->pPrev <> 0 And pTemp->Tag0 <> str_Tag  AND pTemp <> this.pGarbage ) : pTemp = pTemp->pPrev : Wend
    Else
        pTemp = this.pNode : If pTemp=0 Then :  pTemp = this.pFirstNode : End If
        If pTemp->pNext <> 0 Then : pTemp = pTemp->pNext : End If
        While (pTemp->pNext <> 0 And pTemp->Tag0 <> str_Tag  AND pTemp <> this.pLastNode ) : pTemp = pTemp->pNext : Wend
    End If   
    If pTemp->Tag0 = str_Tag Then
        this.pSearchNode=pTemp : this.bSearchRes=1 : If this.bAutoCursor=1 Then : pNode=pTemp : End If : Return 1
    Else : this.bSearchRes = 0 : Return 0 : End If   
End Property

Property List.HasHashTag(str_Tag As String) As Byte
    Dim As zString Ptr  zp1, zp2
    Dim pContextRetour As ListContext
    Dim HadHashTag As Byte=0 : Dim IsEtoile As Byte=0 : Dim i as uByte=1 : Dim t as uByte=Len(str_Tag) : Dim istep As uByte=this.bHashLen
    pContextRetour.pNode=pNode : pContextRetour.pFirstNode=This.pFirstNode : pContextRetour.pLastNode=This.pLastNode : pContextRetour.uCount=This.uCount
    pFirstNode->BranchCount=uCount : pFirstNode->pBranchLastNode=pLastNode : pFirstNode=pFirstFIRSTNode : pNode=pGarbage '   
    this.uCount = this.pFirstNode->BranchCount : pLastNode=this.pFirstNode->pBranchLastNode :' uB_tmp=uB_IsTree : uB_IsTree=1
    zp1=Listptemp+Len(str_Tag)+1 : (*zp1)[0]=0 : *Listptemp=str_Tag : zp1=Listptemp' * Alternative to Mid
    Do
        zp2=zp1 : zp1+=istep : (*zp3)[0]=(*zp1)[0] : (*zp1)[0]=0 : Str_tmp=*zp2 : (*zp1)[0]=(*zp3)[0] ' * Alternative to Mid : Str_tmp=Mid(str_Tag,i, istep)
        If this.HasTagTree(Str_tmp)=1 Then           
            this.pNode = this.pSearchNode
            # IF TagMode=0
                If pNode->ListData<>0 Then : str_testTMP=*this.pNode->ListData->str_tag_C(1) : Else : str_testTMP="" :  End If
            # ELSE
                If pNode->ListData<>0 Then : str_testTMP=this.pNode->ListData->str_tag_C(1) : Else : str_testTMP="" :  End If
            # ENDIF
            If str_testTMP="*" Then : HadHashTag=1 : IsEtoile=1 : i=t
            ElseIf str_testTMP="!*" Then : HadHashTag=0 : i=t
            ElseIf str_testTMP="!" And i=t Then : HadHashTag=0
            ElseIf i>=t Then : HadHashTag=1
            Else :  HadHashTag=0 : End If   
        ElseIf IsEtoile=0 Then : HadHashTag=0 : i=t
        End If
        If i<t Then           
            If this.pNode->pBranch=0 Then
                If bAutoCursor<3 Then : pNode=pContextRetour.pNode : This.pFirstNode=pContextRetour.pFirstNode : This.pLastNode=pContextRetour.pLastNode : This.uCount=pContextRetour.uCount : End If
              '  If bHashKeyUnique=1 Then : uB_IsTree=uB_tmp : Return 1 : Else : uB_IsTree=uB_tmp : If bHashStepRev=1 Then : Return this.KeyStepRev : Else : Return this.KeyStep :  End If  : End If      KO !           
            Else : this.pFirstNode = this.pNode->pBranch : this.uCount = this.pFirstNode->BranchCount : this.pLastNode = this.pFirstNode->pBranchLastNode : this.pNode = this.pFirstNode
            End If : this.bSearchRes = 0
        End If
        i+=istep
    Loop Until i>t ': uB_IsTree=uB_tmp
    If HadHashTag=1 Then : bSearchRes=1 : pSearchNode=pNode
        If bAutoCursor=1 Then : Return 1
        ElseIf bAutoCursor=2 Then : If pNode->Tag1=0 Then : HadHashTag=0 : Else : Return 1 : End If
        End If
    End If
    pNode=pContextRetour.pNode : This.pFirstNode=pContextRetour.pFirstNode : This.pLastNode=pContextRetour.pLastNode : This.uCount=pContextRetour.uCount
    Return HadHashTag   
End Property

Property List.HasKey(str_Tag As String) As Byte
    Dim pContextRetour As ListContext
    pContextRetour.pNode=pNode : pContextRetour.pFirstNode=This.pFirstNode : pContextRetour.pLastNode=This.pLastNode : pContextRetour.uCount=This.uCount
    this.Root : If this.HasHashTag(str_Tag)=1 Then : If pNode->Tag1<>0 And pNode->Tag1<>255 Then : Return 1 : Else : Return 0 : End If : Else : Return 0 : End If
    pNode=pContextRetour.pNode : This.pFirstNode=pContextRetour.pFirstNode : This.pLastNode=pContextRetour.pLastNode : This.uCount=pContextRetour.uCount
End Property
Property List.BranchCount As uInteger : If this.pNode->pBranch<>0 Then : Return pNode->BranchCount : Else : Return 0 : End If : End Property ' Return this.pNode->BranchCount

Property List.NodeFlat As Byte 'Réallocation sélective dynamique multimodes rétro-récursive (virtuelle/reelle glissante 0/-n) de la structure de l'index en memoire virtuelle (Reduce) - Compatible HashStep, HashStepRev(?), KeyStep, KeyStepRev(?), fStep, TrackStep
    Dim As ListContext pContext, pContextRetour : Dim  As ListNode Ptr pTemp1, pTemp2, pTemp3, pTemp4, pTemp5, pTemp6, pTemp7 : Dim As uByte IsLastNode=0 , ubtest=0
    this.NodeRecycle : NodeRecycle2
    'Contrôle multimode en entrée + contrôle du Token de fin de liste + gestion continuité des ptr
    If pNode->Tag0=LIST_RES Or pNode->Tag0=LIST_DEL Or pNode=pWhyteMove Or pNode=pGarbage Then : Return 0    
    ElseIf bTracking=1 Then
        If pFirstNode->pBranch=pFlatRoot Then : Return 0 : End If 
        If pFirstNode->Tag0<>LIST_RES Then : bAlrdyTracked=0 : this.TrackCompute : End If  ' If bTracking=1 Then : bAlrdyTracked=0 : tthis.TrackCompute : End If 
        pTemp6=pNode->pBranchLastNode
    ElseIf pFirstNode->pPrev=0 And pNode->pNext=0 Then : pTemp1=pNode : this.AllOf : pNode=pTemp1
    End If  
    If bRecur=2 And pNode->Tag1<>0 And pNode->Tag1<>255 Then : this.bNFmethod=1
    ElseIf bRecur=3 Then : If pNode->Tag1<>0 And pNode->Tag1<>255 Then : If bFullRecursive=0 Then : Return 0  : End If : this.bNFmethod=1 : Else : this.bNFmethod=-1  : End If
    ElseIf bRecur=4 Then : this.bNFmethod=-1 : End If ' If bTracking=1 Then : bAlrdyTracked=0 : this.TrackCompute : End If : Return 0 : End If  bRecur=0 :
    pTemp5=pFirstNode : pTemp2 = this.pNode : Str_tmp=this.HashTag : pLatestHTag=0 
    'Gestion (swapping) des nodes parents                                                   ---------------------------------------------------------------------------
    If this.pNode->pBranch<>0  Then
        'Validation du mouvement mémoire : ' Fonctionnalité Tree List =>FlatList / ByPass Garbage ou 'Déjà swappé  bNFflag
        If this.bNFmethod<>1 Then : If bTracking=1 Then : this.TrackCompute : End If : Return 0 : ElseIf this.pNode->Tag1=255 Then : Return 0 : ElseIf bHashStepRev=1 And pNode->pNext->Tag0=LIST_DEL Then : Return 0 : End If   ' PATCHED !!
        pTemp1=AllowCake : pTemp4=this.pFlatRoot->pBranch : If this.pNode->pPrev<>0 Then : this.pNode->pPrev->pNext=pTemp1 : End If
        If this.pNode->pNext->pPrev=this.pNode Then this.pNode->pNext->pPrev=pTemp1 : End If
        pTemp1->pPrev=this.pNode->pPrev : pTemp1->pNext=this.pNode->pNext : pTemp1->pBranch=this.pNode->pBranch
        pTemp1->BranchCount=this.pNode->BranchCount : pTemp1->Tag0=this.pNode->Tag0 : pTemp1->Tag1=255
        pTemp1->pBranchLastNode=this.pNode->pBranchLastNode
        If pTemp2=pLastNode Then : pLastNode=pTemp1 : End If
        this.pNode->pBranch->pBranch=pTemp1 : this.pNode->pBranch=0
        If this.pNode->ListData=0 Then : this.pNode->ListData=this.AllowPanCake : End If : pCurrentPanCakeTMP=pNode->ListData : this.FlatTagSet(Str_tmp)  ' *this.pNode->ListData->str_flat_tag = Str_tmp
        this.pNode->pPrev=pTemp4 : pTemp4->pNext->pPrev=this.pNode : this.pNode->pNext=pTemp4->pNext : pTemp4->pNext=this.pNode : this.pNode=pTemp1       
    Else : this.uCount-=1 : pFirstNode->BranchCount-=1 :  If bBranchCountDown=1 Then : this.BCountDown(-1) : End If
    End If
    'Gestion (/optimisation) du parsing (rétro-récursivité) ds le HashStep       ---------------------------------------------------------------------------       
    If pNode->pPrev<>pFirstNode Then
        If this.pNode->pNext->Tag0=LIST_DEL Then : This.pLastNode=this.pNode->pPrev : This.pFirstNode->pBranchLastNode=this.pNode->pPrev : End If
        If bHashStepRev=1 Then : pContextRetour.pNode=pNode->pNext : Else : pContextRetour.pNode=pNode->pPrev :  End If
        pContextRetour.pFirstNode=This.pFirstNode : pContextRetour.pLastNode=This.pLastNode : pContextRetour.uCount=This.uCount   
    Else
        pContext.pNode=This.pNode : pContext.pFirstNode=This.pFirstNode : pContext.pLastNode=This.pLastNode : pContext.uCount=This.uCount       
        If pNode=pLastNode Then : pTemp3=this.pFirstNode->pBranch->pPrev :  Else : pTemp3=this.pFirstNode->pBranch : End If
        If pTemp3->Tag0=LIST_RES Then
            If pNode=pLastNode Then : this.pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : pLastNode=pFirstNode->pBranchLastNode : End If
            pTemp3=this.pFirstNode->pBranch : 
        End If
        this.pNode=pFirstNode->pBranch : pFirstNode=pFirstNode->pPrev : pLastNode=pFirstNode->pBranchLastNode : uCount=pFirstNode->BranchCount ' This.UpLevel         
        pContextRetour.pNode = pTemp3 : pContextRetour.pFirstNode = This.pFirstNode : pContextRetour.pLastNode = This.pLastNode : pContextRetour.uCount=This.uCount
        This.pNode=pContext.pNode : This.pFirstNode=pContext.pFirstNode : This.pLastNode=pContext.pLastNode : this.uCount=pContext.uCount                 
    End If
    If this.pNode->pNext=0 And this.pNode->pPrev->Tag0=LIST_RES Then
        pLocalRoot=this.pNode->pPrev : IsLastNode=1 :  pLocalRoot->Tag1=0 :
        If this.pFirstNode->pBranchLastNode=0 Then : pLocalRoot = this.pFirstNode : pLocalRoot->pBranch->pBranch=0 :  pLocalRoot->Tag1=0 : pLocalRoot->Tag0=LIST_DEL : End If
    ElseIf this.pNode=this.pFirstNode->pBranchLastNode Then       
        If this.pNode->pPrev->Tag0=LIST_RES Then : pLocalRoot=this.pNode->pPrev : IsLastNode=1 : pLocalRoot->Tag1=0 :  pLocalRoot->Tag0=LIST_DEL
        Else : this.pLastNode=this.pNode->pPrev : pFirstNode->pBranchLastNode=pLastNode : IsLastNode=1
        End If
    End If
   
  ' Swapping / MAJ des pointeurs - depend de IsLastNode                        --------------------------------------------------------------------------     
  ' Envoi d'un ancien node parent déjà swappé vers le garbage collector OU envoi d'un node non parent vers le GarbageCollector (si NFmethod=-1)
    If (pTemp2->Tag1=255 Or this.bNFmethod=-1Or this.bNFmethod=4) And pTemp2->pBranch=0   Then
        If IsLastNode=0 Then : pTemp2->pNext->pPrev=pTemp2->pPrev : pTemp2->pPrev->pNext=pTemp2->pNext : End If
        pFlatRoot->pNext->pPrev=pTemp2 : pTemp2->pPrev=pFlatRoot : pTemp2->pNext=pFlatRoot->pNext : pFlatRoot->pNext=pTemp2
        pTemp2->Tag0=LIST_DEL : pTemp2->Tag1=0 : uGarbCt+=1 ' : pTemp2->pBranchLastNode=0
        If pTemp2->ListData<>0 Then : pTemp2->ListData->pNextContainer=pPanCakeGarbage->pNextContainer : pPanCakeGarbage->pNextContainer=pTemp2->ListData : pTemp2->ListData=0 : uContainerGarbCt+=1 : End If
        This.pNode=pContextRetour.pNode : This.pFirstNode=pContextRetour.pFirstNode : This.pLastNode=pContextRetour.pLastNode : This.uCount=pContextRetour.uCount
        If IsLastNode=0 And  bHashStepRev=1 Then : If this.Up=0 Then : If pNode=pNode->pNext <>0 Then : pNode=pNode->pNext  : End If : End If
        ElseIf IsLastNode=1 Then ': bNFmethod=bRecur 
            this.pNode=pTemp5->pBranch : this.uCount=pTemp5->pPrev->BranchCount : pLastNode=pTemp5->pPrev->pBranchLastNode : this.pFirstNode = pTemp5->pPrev           
            If bTracking=1 Or bPickReduce=1 Then : If bNFmethod=-1 Then : this.NodeRecycle2 : ElseIf bNFmethod>1 Then : bRecur=bNFmethod : bNFmethod=-1 : End If : If pNode->Tag1=0 Or pNode->Tag1=255 Or bRecur>1 Then : this.NodeFlat : this.NodeRecycle2 : If bRecur>1 Then : bNFmethod=bRecur : End If : End If ' RECURSIF
            ElseIf bHashStepRev=1 Then : If this.Up=0 Then : this.Last : End If : this.NodeRecycle2 : Return 1
            Else : If pFirstNode<>pFirstFIRSTNode Then : this.UpLevel : Else : pNode=pGarbage : End If  : this.NodeRecycle2 : Return 1 : End If
        End If
        If bFullRecursive=1 Then : this.up : this.NodeFlat : this.NodeRecycle2 : End If '                   *********** FULL REC
    Else ' Envoi vers la Flat list - Optimisation, à voir                                      --------------------------------------------------------------------------                 
        pFlatRoot->pBranch->BranchCount+=1         
        pNode=pFirstNode->pNext : this.UpLevel
        this.pFirstNode = this.pFirstFIRSTNode : this.pLastNode = this.pFirstNode->pBranchLastNode : this.uCount=this.pFirstNode->BranchCount
        this.pNode=this.pFlatRoot : this.Branch
        If pFlatRoot->pBranch->BranchCount=0 Then : pFlatRoot->pBranch->BranchCount=1 : this.uCount=1 :  End If
        pTemp1 = this.pLastNode
        If IsLastNode=0 Then            
            If pTemp2->pNext<>0 Then : pTemp2->pNext->pPrev=pTemp2->pPrev :  End If
            If pTemp2->pPrev<>0 And pTemp2->Tag0<>LIST_RES Then : pTemp2->pPrev->pNext=pTemp2->pNext : End If
            If pTemp2->ListData=0 Then : pTemp2->ListData=this.AllowPanCake : End If : pCurrentPanCakeTMP=pTemp2->ListData : this.FlatTagSet(Str_tmp)
            pTemp1->pNext = pTemp2 : this.pLastNode=pTemp2 : this.pFirstNode->pBranchLastNode=pTemp2 : pTemp2->pPrev=pTemp1 : pTemp2->pNext=0            
            this.pNode=pContextRetour.pNode : This.pFirstNode=pContextRetour.pFirstNode : This.pLastNode=pContextRetour.pLastNode : This.uCount=pContextRetour.uCount
            If bFullRecursive=1 Then : this.up : this.NodeFlat : this.NodeRecycle2 : End If '                   *********** FULL REC
            If bHashStepRev=1 Then : If this.Up=0 Then : this.BlindStep : End If : If pNode=pNode->pNext <>0 Then : pNode=pNode->pNext : End If : Return 1 :  End If               
        Else
            If pTemp2->ListData=0 Then : pTemp2->ListData=this.AllowPanCake : End If : pCurrentPanCakeTMP=pTemp2->ListData : this.FlatTagSet(Str_tmp)
            pTemp1->pNext=pTemp2 : this.pLastNode=pTemp2 : this.pFirstNode->pBranchLastNode = pTemp2 : pTemp2->pPrev=pTemp1 : pTemp2->pNext=0
            this.pNode=pTemp5->pBranch : this.pFirstNode = pTemp5->pPrev : pLastNode=pTemp5->pPrev->pBranchLastNode : this.uCount=pTemp5->pPrev->BranchCount            
            If bTracking=1 Or bPickReduce=1 Then : If bNFmethod<2 Then : this.NodeRecycle2 : ElseIf bNFmethod>1 Then : bRecur=bNFmethod : bNFmethod=-1 : End If : If pNode->Tag1=0 Or pNode->Tag1=255 Or bRecur>1 Then :  this.NodeFlat :  this.NodeRecycle2  :  If bRecur>1 Then : bNFmethod=bRecur : End If : End If  ' RECURSIF
            ElseIf bHashStepRev=1 Then : If this.Up=0 Then : this.Last : End If : this.NodeRecycle2 : If pNode=pNode->pNext <>0 Then : pNode=pNode->pNext : End If  : Return 1
            Else : If pFirstNode<>pFirstFIRSTNode Then : this.UpLevel : Else : pNode=pGarbage : End If  : this.NodeRecycle2 : Return 1 : End If
        End If
    End If
    If bTracking=1 And pTemp6<>0 Then 
        If pFirstNode->Tag0<>LIST_RES Then : bAlrdyTracked=0 : this.TrackCompute : End If
        this.NodeRecycle : pNode=AllowCake : pNode->pBranchLastNode=pTemp6 : pLocalMove=pNode : pLocalMove->Tag0=LIST_DEL
        Return 1 
    End If
    Return 1
End Property

Property List.RestoreHash As Byte
    Dim As ListNode Ptr pTemp,  pTmpPrev, pTmpNext, pMove
    Dim str_tmp as string : Dim bTagExists As Byte : Dim As Byte uTagTmp=uTag, IsLastOne=0
    If this.pNode->ListData=0 Then : Return 0 : End If ' If pNode=pLocalMove Then : Return 0 : Else  Print "LZLE : RestoreHash : failure or non-value" :
    pTemp=this.pnode
    # IF TagMode=0
        If uTag=0 Then : str_tmp=*this.pNode->ListData->str_flat_tag : Else : str_tmp=*this.pNode->ListData->str_tag_C(uTag) : End If
    # ELSE
        If uTag=0 Then : str_tmp=*this.pNode->ListData->str_flat_tag : Else : str_tmp=this.pNode->ListData->str_tag_C(uTag) : End If
    # ENDIF
    If str_tmp="" Then : Return 0 : End If : pTmpPrev=this.pnode->pPrev : pTmpNext=this.pnode->pNext
    'Vérification du contexte
    If pNode->pNext=pEndFlat And pNode->pPrev=pFirstNode Then : IsLastOne=1 : End If
    this.NodeRecycle
    If this.pNode=this.pEndFlat Or pFirstNode->pBranch<>pFlatRoot  Then : this.Up : Return 0 : End If     
    'HashTagging
    uTag=0 : bRHByPass=1 : bTagExists=this.HashTag(str_tmp) : bRHByPass=0     
    If bHashKeyUnique=0 Then 'Multikeys=>1 seul mode possible car pas de conflit possible
        If uTag=0 Then : pTemp->Tag0= this.pnode->Tag0  : Else : pTemp->Tag0= Str(this.pnode->Tag1)  : End If
        pMove=this.pnode : pCurrentPanCakeTMP=pMove->ListData : this.FlatTagSet(str_tmp) ' *pMove->ListData->str_flat_tag=str_tmp :
        bTagExists=3 ' swap
    ElseIf bTagExists<>0 And pNode->Tag1<>0  Then ' tag deja existant et "vraie" clef
        'Gestion des modes (RHmethod)
        If uTag=0 Then
            If bRHmethod=-1 Then : pTemp->Tag0= this.pnode->Tag0 : pLocalMove=this.pnode : pLocalMove->Tag0=LIST_DEL : this.pFlatRoot->pBranch->BranchCount-=1 ':  pLocalMove->pBranch=0  :'envoi garbage
            ElseIf bRHmethod=1 Then : pTemp->Tag0= this.pnode->Tag0  : pMove=this.pnode :  pCurrentPanCakeTMP=pMove->ListData : this.FlatTagSet(str_tmp) : bTagExists=2 ' swap *pMove->ListData->str_flat_tag=str_tmp
            Else : If pNode->Tag1=255 Then : pNode->Tag1=1 : End If : this.RootPrivate : this.pNode=this.pFlatRoot : this.Branch : pNode=pTemp : Return 0  ' bypass
            End If   
        Else
            If bRHmethod=-1 Then : pTemp->Tag0= Str(this.pnode->Tag1)  : pLocalMove=this.pnode : pLocalMove->Tag0=LIST_DEL : this.pFlatRoot->pBranch->BranchCount-=1 ':  pLocalMove->pBranch=0  :'envoi garbage
            ElseIf bRHmethod=1 Then : pTemp->Tag0= Str(this.pnode->Tag1)  : pMove=this.pnode : pCurrentPanCakeTMP=pMove->ListData : this.FlatTagSet(str_tmp) : bTagExists=2 ' swap *pMove->ListData->str_flat_tag=str_tmp :
            Else : If pNode->Tag1=255 Then : pNode->Tag1=1 : End If : this.RootPrivate : this.pNode=this.pFlatRoot : this.Branch : pNode=pTemp : Return 0  ' bypass
            End If   
        End If
    Else
        If uTag=0 Then : pTemp->Tag0= this.pnode->Tag0 : Else : pTemp->Tag0=Str(this.pnode->Tag1) : End If
        pLocalMove=this.pnode : pLocalMove->Tag0=LIST_DEL : this.pFlatRoot->pBranch->BranchCount-=1 ':  pLocalMove->pBranch=0  :
    End If
    uTag=uTagTmp
    'Swapping mémoire
    pNode->Tag1=pTemp->Tag1
    pTemp->pPrev->pNext=pTemp->pNext : pTemp->pNext->pPrev=pTemp->pPrev
    pTemp->pPrev=this.pnode->pPrev : pTemp->pNext=this.pnode->pNext  : *pTemp->ListData->str_flat_tag=""
  '  If bNFmethod<>3 Then : pTemp->pBranch=this.pnode->pBranch : End If  ' Deprecated : "flat" list is not designed to support a hierarchy - would be too complex to manage - use snatch instead
    this.pnode->pPrev->pNext=pTemp
    If this.pnode->pNext<>0 Then : If this.pnode->pNext->pPrev=this.pnode Then : this.pnode->pNext->pPrev=pTemp :  End If : End If
    If this.pnode=this.pLastNode Then : this.pLastNode=pTemp : End If : If this.pnode=this.pFirstNode Then : this.pFirstNode=pTemp : End If     
    If this.pnode->pBranch<>0 Then  '  pFirstNode->Tag1=""  '  pnode->pBranch->Tag1=""
        If this.pnode=this.pFirstNode->pBranchLastNode Then : this.pFirstNode->pBranchLastNode=pTemp : End If
        If this.pnode->pBranch->pBranch=this.pnode Then : this.pnode->pBranch->pBranch=pTemp : End If       
        pTemp->pBranch=this.pnode->pBranch : pTemp->pBranchLastNode=this.pnode->pBranchLastNode : pTemp->BranchCount=this.pnode->BranchCount
    End If     
    'Reprise ds Flat List
    this.FlatStack(1)
    If bTagExists<>3 Then : this.pnode=pLocalMove : this.pnode->pPrev=pTmpPrev : this.pnode->pNext=pTmpNext :  pLocalMove->pBranch=0  : pLocalMove->Tag1=0 : pLocalMove->Tag0=LIST_DEL :
    Else : pNode=pMove : pTmpPrev->pNext=pNode : pTmpNext->pPrev=pNode : pNode->pPrev=pTmpPrev : pNode->pNext=pTmpNext :
    End If : If IsLastOne=1 Then : this.Up : bfStepZero=1 : pNode=pLastNode : End If '
    Return 1
End Property

Property List.Check(ub As uByte) As Byte : this.pNode->Tag1=ub : Return 1 : End Property
Property List.Check As Byte : Return this.pNode->Tag1 : End Property

'==========================================================================================TYPE LIST PUBLIC PROPERTIES - PROPERTIES PARAMETERS
Property List.AutoCursor(b As Byte) As Byte : If 0<=b<=3 Then : bAutoCursor=b : Return 1 : End If : Return 0 : End Property
Property List.BranchCountDown(i as Byte) As Byte
    If i<>0 And i<>1 Then : Print "BranchCountDown : invalid parameter" : Return 0 : ElseIf i=0 Or (i=1 And bColBcountLocked=0) Then : bBranchCountDown=i : bColBcountLocked=i : Return 1 : Else : Print "Branch count vector already booked - use 'VectorUnlock' property before" : Return 0 : End If
End Property
Property List.CopyCatMethod(i As Byte) As Byte : If i=0 Or i=1 Then : bCopyCatMethod=i : Return 1 : Else : Print "CopyCatMethod : invalid parameter" : Return 0 : End If : End Property
Property List.HashKeyUnique(ub as Byte) As Byte : If ub=0 Or ub=1 Then : bHashKeyUnique=ub : bSeekMethod=ub : Return 1 : Else : Print "HashKeyUnique : invalid parameter" :  Return 0 : End If : End Property
Property List.HashLen(bHashLen As uByte) As Byte : If bHashLen>MAX_HASHLEN Then : Print "LZLE warning : HashLen Property ignored (deprecated), or must be =< MAX_HASHLEN (constant)" : End If : this.bHashLen=1 : Return 1 : End Property
Property List.KeysRegister(ub As uByte) As Byte : Print "LZLE warning : KeysRegister Property ignored (deprecated)" :  Return 0 : End Property
Property List.NFmethod(i As Byte) As Byte : If i=-1 Or i=0 Or i=1 Or i=2 Or i=3 Or i=4 Then : this.bNFmethod=i : Return 1 : Else : Print "NFmethod : invalid parameter" : Return 0 : End If : End Property
Property List.NFrecursive(i As Byte) As Byte : bFullRecursive=0 :  If i=2 Then :  this.bPickReduce=1 : this.bFullRecursive=1 : ElseIf i=1 Then : this.bPickReduce=1 : Else : this.bPickReduce=0 : If i<>0 Then :  ? "LZLE warning : NFrecursive : invalid parameter - autoset to 0" : End If : End If : Return 1 : End Property
'Property List.NFFullRecursive(i As Byte) As Byte : If i=1 Then : this.bFullRecursive=1 : Else : this.bFullRecursive=0 : If i<>0 Then :  ? "LZLE warning : NFFullRecursive : invalid parameter - autoset to 0" : End If : End If : Return 1 : End Property
Property List.PVSmethod(ub As Byte) As Byte :
    If ub=-1 Then : bPVSmethod=ub : Return 1
    ElseIf bColBcountLocked=0 and ub<2 Then : bPVSmethod=ub : bColBcountLocked=1 : Return 1
    Else : Print "PVSmethod : Branch count vector already booked or invalid parameter" : Beep : sleep : Return 0 : End If
End Property
Property List.PVSratio(ub As uByte) As Byte : If ub>1 Then : PVS_ratio=ub : Return 1 : Else : Return 0 : End If : End Property
Property List.RHmethod(i As Byte) As Byte : If -2<i<2 Then : this.bRHmethod=i : Return 1 : Else : this.bRHmethod=-1 : Return 0 : End If : End Property
Property List.SeekMethod(i as Byte) As Byte : If i=0 Or i=1 Or i=2 Then : this.bSeekMethod=i : Return 1 : Else : ? "LZLE warning : SeekMethod : invalid parameter" : Return 0 : End If : End Property
Property List.SnatchBLmethod(i As Byte) As Byte : If i=0 Or i=1 Then : bSnatchBLMethod=i : Return 1 : Else : Print "LZLE warning : SnatchBLMethod : invalid parameter" : Return 0 : End If : End Property
Property List.TrackMethod(by As Byte) As Byte : ? "LZLE warning : TrackMethod Property deprecated (ignored, autoset)" : Return 0 : End Property ' If by=0 Or by=1 Then : bTrackingMethod=by : Return 1 : Else : Return 0 : End If
Property List.TrackMultiKeys(uB as uByte) As Byte : If -1<=uB<=MAX_COLS+1 Then : bTrackMultiKeys=uB : Return 1 : End If : ? "LZLE warning : TrackMultiKeys : invalid parameter" : Return 0 : End Property
Property List.TrackTarget(uB As uByte) As Byte : If 0<=uB<=MAX_TRACKS Then : uBTrackTarget=uB : Return 1 : End If : ? "LZLE warning : TrackTarget : invalid parameter" : Return 0 : End Property
Property List.VectorUnlock As Byte : bColBcountLocked=0 : bColTrackLocked=0 : Return 1 : End Property
Property List.VectorClear As Byte
    Dim As ListNode Ptr Ptemp1=pNode, pTemp2 : Dim As Byte IsUp=0
    pTemp2=pTemp1->pNext
    If pTemp2=0 Or pNode->Tag0=LIST_RES  Or pNode->Tag0=LIST_DEL Then : Return 0 : End If
    If bColBcountLocked=1 Or bPVSmethod=1 Then : While this.HashStepTrace And pNode<>pTemp2 :  If bHStepTrace=-1 Then : pFirstNode->Tag1=0 : End If : pNode->BranchCount=0 : Wend
    Else : While this.HashStepTrace And pNode<>pTemp2 :  If bHStepTrace=-1 Then : pFirstNode->Tag1=0 : End If : Wend
    End If : pNode=pTemp1 : Return 1
End Property

'==========================================================================================TYPE LIST PUBLIC PROPERTIES - FLOW CONTROL
Property List.Up As Byte : If pFirstNode=pFirstFIRSTnode Then : Return 0 : Else :  this.UpLevel : Return 1: End If : End Property
Property List.Down As Byte
    If pnode->pBranch=0 Or pNode->Tag0=LIST_RES Then  : Return 0 : End If
    If pnode->pBranch->pPrev<>pFirstNode Then : Return 0 :
    Else : pFirstNode->BranchCount=uCount : pFirstNode->pBranchLastNode=pLastNode : pFirstNode=pNode->pBranch : uCount=pFirstNode->BranchCount : pLastNode=pNode->pBranch->pBranchLastNode : pNode=pNode->pBranch : Return 1
    End If
End Property
Property List.HoldBack As Byte : Return this.HoldBack(0) : End Property
Property List.HoldBack(by As Byte) As Byte
    uBHoldBackRev=0
    If pNode->Tag0=LIST_RES Or bColTrackLocked=1 Then : Return 0
    ElseIf TrackTrace(by)<>0 Then : TrackTrace(by)->pBranchLastNode=pNode : TrackTrace(by)=this.pNode : Return 1
    Else : this.TrackSet(by) :TrackTrace(by)=pNode : Return 1 '  pNode->pBranchLastNode=0 : 
    End If
End Property
Property List.HoldBackRev As Byte : Return this.HoldBackRev(0) : End Property
Property List.HoldBackRev(by As Byte) As Byte
    uBHoldBackRev=1
    If pNode->Tag0=LIST_RES Or bColTrackLocked=1 Then : Return 0
    ElseIf TrackTrace(by)<>0 Then : pNode->pBranchLastNode=TrackTrace(by) : TrackTrace(by)=this.pNode : pLastHoldBackRev(by)=pNode : Return 1
    Else : this.TrackSet(by) : TrackTrace(by)=pNode ': pNode->pBranchLastNode=pFirstNode :
    Return 1
    End If
End Property

Property List.TrackStep As Byte
    bAlrdyTracked=0    
    If pNode->pBranchLastNode->Tag0=LIST_DEL Or bColTrackLocked=1 Or pNode->pBranchLastNode=0 Then : this.Root : bTracking=0 :  Return 0 : End If
    If pTrackTmp=pNode Then  ' If pNode->pBranchLastNode=0 Then : this.Root : bTracking=0 : Return 0  : End If
        pFirstNode=pFirstNodeTMP : pLastNode=pLastNodeTMP : this.uCount=pFirstNode->BranchCount 
    End If    
    If pNode->pBranchLastNode<>0  And pNode->pBranchLastNode->Tag0<>LIST_RES Then 
        pTrackTmp=pNode : pNode=pNode->pBranchLastNode : bTracking=1 
        pFirstNodeTMP=pFirstNode : pLastNodeTMP=pLastNode : pTrackPrevTmp=pTrackTmp->pPrev ': pTrackNextTmp=pTrackTmp->pNext 
        If pFirstNode->Tag0<>LIST_RES Then : bAlrdyTracked=0 : this.TrackCompute ' bTrackingMethod=1 Or 
        ElseIf pNode->pNext=pTrackTmp Or pNode=pTrackPrevTmp  Then : bAlrdyTracked=1: End If ' Or pNode->pNext=pTrackNextTmp Ko
        If pNode=pEndFlat Then : this.Root : bTracking=0 :  Return 0 : End If : this.TrackSecure
        Return 1
    End If : bAlrdyTracked=0 : this.TrackCompute : bTracking=0 : Return 0
End Property

Property List.Track As Byte : Return this.Track(0) : End Property  'ubTrackTarget
Property List.Track(by As Byte) As Byte
    this.Root : If bColTrackLocked=1 Then : Print "Tracking vector already in use (internal) by CopyCat/Follow" & chr(10) & "Use HashStep or other iterator" & chr(10) & "Attempt to use incompatible vectors may lead to crash" : Return 0 : End If : bAlrdyTracked=0
    If pNode->Tag0<>LIST_DEL And pNode->Tag0<>LIST_RES And pNode<>pLocalMove Then : This.TrackSecure : End If : this.Root : bTracking=1 ': This.TrackMethod(1)
    If this.Tracks(by).pNode=0 Then : this.TrackSet(by)  : Return 0
    Else
        pFirstNode->BranchCount=this.uCount : pFirstNode->pBranchLastNode=pLastNode : this.NodeRecycle : pNode=AllowCake : pLocalMove=pNode : pLocalMove->Tag0=LIST_DEL
        If this.Tracks(by).pNode->Tag0=LIST_DEL Then 
            If uBHoldBackRev=0 Then : pNode->pBranchLastNode=this.Tracks(by).pNode->pBranchLastNode
            Else : pNode->pBranchLastNode=TrackTrace(by)->pBranchLastNode : End If
        Else 
            If uBHoldBackRev=0 Then : pNode->pBranchLastNode=this.Tracks(by).pNode
            Else : pNode->pBranchLastNode=TrackTrace(by) : End If
        End If
        If uBHoldBackRev=0 Then : pFirstNode=this.Tracks(by).pFirstNode : pLastNode=pFirstNode->pBranchLastNode : bHashLen=this.Tracks(by).bLcHashLen
        Else : bAlrdyTracked=0 : this.TrackCompute : End If
        pNode->pBranch=0 : pNode->pPrev=0 : pNode->Tag0="" : bAlrdyTracked=0  ': bTracking=1 
        Return 1
    End If
End Property

Property List.TrackSet As Byte : Return this.TrackSet(0) : End Property
Property List.TrackSet(by As Byte) As Byte 
    Dim pTemp As ListNode Ptr
    If uBHoldBackRev=0 Then : pTemp=this.Tracks(by).pNode : Else : pTemp=TrackTrace(by) : End If
    If pNode->Tag0=LIST_RES Or pNode->Tag0=LIST_DEL Or pNode->Tag0=LIST_RES Or pNode->Tag0="" Then
        If this.Tracks(by).pFirstNode=0 Then : Return 0 : Else : ? "LZLE error : Invalid TrackSet Context" : Return 0 : End If
    End If
    this.NodeRecycle :  this.Tracks(by).pNode=pNode 
    pTrackTmp=0 : bAlrdyTracked=0 : this.TrackSecure : bTracking=1
    this.Tracks(by).pFirstNode=pFirstNode : this.Tracks(by).bLcHashLen=bHashLen : TrackTrace(by)=pNode : 
    Return 1
End Property

Property List.TrackClear As Byte : Return this.TrackClear(0) : End Property
Property List.TrackClear(by As Byte) As Byte : TrackTrace(by)=0 : Return 1  : End Property
#endif

end

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
