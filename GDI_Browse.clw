   PROGRAM


INCLUDE('_photon_.inc'),ONCE

   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

   MAP
     MODULE('GDI_BROWSE_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('GDI_BROWSE001.CLW')
Main                   PROCEDURE   !
     END
     MODULE('Win32Api')
     	SA_DrawText(LONG hdc, *CString lpszString, unsigned nCount, *SA_RECT lpRect, |
     		unsigned uFormat),signed,raw,pascal,proc,name('DrawTextA')
     
     	SA_GetTextExtentPoint(LONG hdc, *cstring lpString, long cbString, *SA_SIZE lpSize),bool,raw,pascal,proc,name('GetTextExtentPointA')
     
     	SA_GetTextExtentPoint32(LONG hdc,           | handle to DC
     	*CSTRING lpString,  | text string
     LONG        cbString,      | characters in string
     	*SA_SIZE lpSize     | string size SIZE structure
     	),BOOL,PASCAL,RAW,PROC,NAME('GetTextExtentPoint32A')
     
     	GetWindowRect(UNSIGNED, *RECT), BOOL, RAW, PASCAL, PROC
     
     	MulDiv(SIGNED, SIGNED, SIGNED), SIGNED, PASCAL
     
     	GetKeyState(SIGNED nVirtKey), SHORT, PASCAL
     
     	GetTickCount(), LONG, PASCAL
     
     	SHBrowseForFolder(LONG lpbi), LONG, PASCAL
     
     	SHGetPathFromIdList(LONG pidl, *CSTRING), BOOL, RAW, PASCAL
     
     	GetSystemMetrics(Long),Long,PASCAL
     END
   END

GLO:Inv_Expand_All   BYTE
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
Invoices             FILE,DRIVER('TOPSPEED'),RECLAIM,OEM,PRE(INV),BINDABLE,CREATE,THREAD !                     
Invoice_Sequence_Key     KEY(INV:Invoice_Sequence),NOCASE,PRIMARY !                     
Record                   RECORD,PRE()
Invoice_Sequence            LONG                           !                     
Name                        STRING(256)                    !                     
Date                        DATE                           !                     
SubTotal                    DECIMAL(12,2)                  !                     
TAX                         DECIMAL(12,2)                  !                     
Total                       DECIMAL(12,2)                  !                     
Note                        STRING(256)                    !                     
Expand                      BYTE                           !                     
                         END
                     END                       

Invoice_Detail       FILE,DRIVER('TOPSPEED'),RECLAIM,OEM,PRE(INDET),BINDABLE,CREATE,THREAD !                     
Invoice_Sequence_Key     KEY(INDET:Invoice_Sequence),DUP,NOCASE !                     
Invoice_Detail_Sequence_Key KEY(INDET:Invoice_Detail_Sequence),NOCASE,PRIMARY !                     
Record                   RECORD,PRE()
Invoice_Sequence            LONG                           !                     
Invoice_Detail_Sequence     LONG                           !                     
Item_Code                   STRING(32)                     !                     
Item                        STRING(256)                    !                     
Description                 STRING(256)                    !                     
Quantity                    DECIMAL(10,2)                  !                     
Price                       DECIMAL(10,2)                  !                     
Extended                    DECIMAL(12,2)                  !                     
                         END
                     END                       

!endregion

Access:Invoices      &FileManager,THREAD                   ! FileManager for Invoices
Relate:Invoices      &RelationManager,THREAD               ! RelationManager for Invoices
Access:Invoice_Detail &FileManager,THREAD                  ! FileManager for Invoice_Detail
Relate:Invoice_Detail &RelationManager,THREAD              ! RelationManager for Invoice_Detail

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               CLASS(INIClass)                       ! Global non-volatile storage manager
Fetch                  PROCEDURE(),DERIVED
Update                 PROCEDURE(),DERIVED
                     END

GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\GDI_Browse.INI', NVD_INI)                 ! Configure INIManager to use INI file
  DctInit()
  Main
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()


INIMgr.Fetch PROCEDURE

  CODE
  GLO:Inv_Expand_All = SELF.TryFetch('Preserved','GLO:Inv_Expand_All') ! Resore ' preserved variable' from non-volatile store
  PARENT.Fetch


INIMgr.Update PROCEDURE

  CODE
  PARENT.Update
  SELF.Update('Preserved','GLO:Inv_Expand_All',GLO:Inv_Expand_All) ! Save 'preserved variable' in non-volatile store

