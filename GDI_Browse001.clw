

   MEMBER('GDI_Browse.clw')                                ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('GDI_BROWSE001.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Frame
!!! </summary>
Main PROCEDURE 

AppFrame             APPLICATION('Application'),AT(,,821,430),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  RESIZE,CENTER,ICON('WAFRAME.ICO'),MAX,STATUS(-1,80,120,45),SYSTEM,IMM
                       MENUBAR,USE(?Menubar)
                         MENU('&File'),USE(?FileMenu)
                           ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('&Edit'),USE(?EditMenu)
                           ITEM('Cu&t'),USE(?Cut),MSG('Cut Selection To Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),MSG('Copy Selection To Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),MSG('Paste From Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Window'),USE(?WindowMenu),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Arrange multiple opened windows'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Arrange multiple opened windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Arrange the icons for minimized windows'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Help'),USE(?HelpMenu)
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('Provides general instructions on using help'), |
  STD(STD:HelpOnHelp)
                         END
                       END
                       TOOLBAR,AT(0,0,821,38),USE(?TOOLBAR1)
                         BUTTON('Invoices'),AT(1,1,107,33),USE(?Invoices_Button)
                       END
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Menu::Menubar ROUTINE                                      ! Code for menu items on ?Menubar
Menu::FileMenu ROUTINE                                     ! Code for menu items on ?FileMenu
Menu::EditMenu ROUTINE                                     ! Code for menu items on ?EditMenu
Menu::WindowMenu ROUTINE                                   ! Code for menu items on ?WindowMenu
Menu::HelpMenu ROUTINE                                     ! Code for menu items on ?HelpMenu

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(AppFrame)                                      ! Open window
  Do DefineListboxStyle
  SELF.SetAlerts()
      AppFrame{PROP:TabBarVisible}  = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    ELSE
      DO Menu::Menubar                                     ! Process menu items on ?Menubar menu
      DO Menu::FileMenu                                    ! Process menu items on ?FileMenu menu
      DO Menu::EditMenu                                    ! Process menu items on ?EditMenu menu
      DO Menu::WindowMenu                                  ! Process menu items on ?WindowMenu menu
      DO Menu::HelpMenu                                    ! Process menu items on ?HelpMenu menu
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Invoices_Button
      START(Invoices, 25000)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Form Invoice_Detail
!!! </summary>
Update_Invoice_Detail PROCEDURE 

CurrentTab           STRING(80)                            ! 
ActionMessage        CSTRING(40)                           ! 
History::INDET:Record LIKE(INDET:RECORD),THREAD
QuickWindow          WINDOW('Form Invoice_Detail'),AT(,,678,111),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  RESIZE,CENTER,GRAY,IMM,MDI,HLP('Update_Invoice_Detail'),SYSTEM
                       BUTTON('&OK'),AT(564,90,49,14),USE(?OK),LEFT,ICON('ok_32x32.png'),DEFAULT,FLAT,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON('&Cancel'),AT(617,90,49,14),USE(?Cancel),LEFT,ICON('cancel_32x32.png'),FLAT,MSG('Cancel operation'), |
  TIP('Cancel operation')
                       ENTRY(@s32),AT(47,10,83,10),USE(INDET:Item_Code)
                       PROMPT('Item Code:'),AT(7,9),USE(?INDET:Item_Code:Prompt),TRN
                       PROMPT('Item:'),AT(146,10),USE(?INDET:Item:Prompt),TRN
                       TEXT,AT(165,10,269,30),USE(INDET:Item)
                       PROMPT('Description:'),AT(124,47),USE(?INDET:Description:Prompt),TRN
                       TEXT,AT(165,47,269,30),USE(INDET:Description)
                       PROMPT('Quantity:'),AT(439,10),USE(?INDET:Quantity:Prompt),TRN
                       ENTRY(@n-14.2),AT(473,10,64,10),USE(INDET:Quantity),DECIMAL(12)
                       PROMPT('Price:'),AT(538,9),USE(?INDET:Price:Prompt),TRN
                       ENTRY(@n$-14.2),AT(561,9,104,10),USE(INDET:Price),DECIMAL(12)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
Update                 PROCEDURE(),DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Record Will Be Added'
  OF ChangeRecord
    ActionMessage = 'Record Will Be Changed'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Update_Invoice_Detail')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?OK
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(INDET:Record,History::INDET:Record)
  SELF.AddHistoryField(?INDET:Item_Code,3)
  SELF.AddHistoryField(?INDET:Item,4)
  SELF.AddHistoryField(?INDET:Description,5)
  SELF.AddHistoryField(?INDET:Quantity,6)
  SELF.AddHistoryField(?INDET:Price,7)
  SELF.AddUpdateFile(Access:Invoice_Detail)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Invoice_Detail.SetOpenRelated()
  Relate:Invoice_Detail.Open()                             ! File Invoice_Detail used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Invoice_Detail
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.CancelAction = Cancel:Cancel+Cancel:Query         ! Confirm cancel
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  IF SELF.Request = ViewRecord                             ! Configure controls for View Only mode
    ?INDET:Item_Code{PROP:ReadOnly} = True
    ?INDET:Quantity{PROP:ReadOnly} = True
    ?INDET:Price{PROP:ReadOnly} = True
  END
  Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)      ! Controls like list boxes will resize, whilst controls like buttons will move
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Invoice_Detail.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  INDET:Extended = INDET:Quantity * INDET:Price
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.Update PROCEDURE

  CODE
  PARENT.Update
  INDET:Extended = INDET:Quantity * INDET:Price


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Invoices file
!!! </summary>
Invoices PROCEDURE 

LOC:Seq              LONG                                  ! 
A:Area               QUEUE,PRE(Q:Area)                     ! 
Y                    LONG                                  ! 
X                    LONG                                  ! 
EX                   LONG                                  ! 
EY                   LONG                                  ! 
Line                 LONG                                  ! 
Pointer              LONG                                  ! 
Field                CSTRING(64)                           ! 
Table                CSTRING(128)                          ! 
                     END                                   ! 
ptOldPoint           GROUP,PRE(PNT)                        ! 
x                    SIGNED                                ! 
y                    SIGNED                                ! 
                     END                                   ! 
LOC:Coords           STRING(64)                            ! 
RGB_GRP              GROUP,PRE()                           ! 
HighByte             BYTE                                  ! 
blue                 BYTE                                  ! 
green                BYTE                                  ! 
red                  BYTE                                  ! 
                     END                                   ! 
HighByte             BYTE                                  ! 
blue                 BYTE                                  ! 
green                BYTE                                  ! 
red                  BYTE                                  ! 
LOC:Bezier           LIKE(SA_POINT),DIM(4)                 ! 
LOC:SearchString     CSTRING(64)                           ! 
LOC:MenuVisible      BYTE                                  ! 
RGB_Address          ULONG                                 ! 
LOC:memoryDC         LONG                                  ! 
LOC:memoryBitMap     LONG                                  ! 
LOC:memoryOriginalBitMap LONG                              ! 
Q:View               QUEUE,PRE(Q:V)                        ! 
Seq                  LONG                                  ! 
Kind                 CSTRING(32)                           ! 
Lines                LONG                                  ! 
PTR                  LONG                                  ! 
                     END                                   ! 
RectVertex           GROUP,PRE(),DIM(2)                    ! 
RVX                  LONG                                  ! 
RVY                  LONG                                  ! 
RVRed                USHORT                                ! 
RVGreen              USHORT                                ! 
RVBlue               USHORT                                ! 
RVAlpha              USHORT                                ! 
                     END                                   ! 
GradientRect         GROUP,PRE()                           ! 
GRUpperLeft          ULONG                                 ! 
GRLowerRight         ULONG                                 ! 
                     END                                   ! 
LOC:WindowHandle     LONG                                  ! 
LOC:ClickedParentID  LONG                                  ! 
LOC:controlDC        LONG                                  ! 
LOC:sourceWidth      LONG                                  ! 
LOC:sourceHeight     LONG                                  ! 
ThisWNDC             LONG                                  ! 
MyWindowDC           UNSIGNED                              ! 
FactorA              DECIMAL(7,2)                          ! 
FactorB              DECIMAL(7,2)                          ! 
FactorC              DECIMAL(7,2)                          ! 
PenHandle            ULONG                                 ! 
FactorD              DECIMAL(7,2)                          ! 
TestRadius           DECIMAL(7,2)                          ! 
LOC:KeyCode          CSTRING(32)                           ! 
mousexpos            SIGNED                                ! 
mouseypos            SIGNED                                ! 
Stepper              LONG                                  ! 
GDI_OK               BYTE                                  ! 
LOC:ToolbarHeight    LONG                                  ! 
LOC:ClickedData      LONG                                  ! 
LOC:ClickedName      STRING(100)                           ! 
LOC:ClickedData_Saved LONG                                 ! 
LOC:ClickedData_Menu LONG                                  ! 
LOC:Parent           LONG                                  ! 
LOC:RebuildResize    BYTE                                  ! 
ThisWallpaper        CSTRING(255)                          ! 
Q:Browse             QUEUE,PRE(Q:BR)                       ! 
SEQ                  LONG                                  ! 
FEQ                  SIGNED                                ! 
                     END                                   ! 
Q:Products_List      QUEUE,PRE(Q:PL)                       ! 
Text                 STRING(1024)                          ! 
NF                   LONG                                  ! 
NB                   LONG                                  ! 
SF                   LONG                                  ! 
SB                   LONG                                  ! 
Is_Header            BYTE                                  ! 
ID                   LONG                                  ! 
Kind                 STRING(2)                             ! 
Pos                  LONG                                  ! 
Trans_ID             LONG                                  ! 
ItemSeq              LONG                                  ! 
                     END                                   ! 
Q:PolyCoords         QUEUE,PRE(QPCoords)                   ! 
ID                   LONG                                  ! 
Xpos                 LONG                                  ! 
Ypos                 LONG                                  ! 
                     END                                   ! 
ID                   LONG                                  ! 
Xpos                 LONG                                  ! 
Ypos                 LONG                                  ! 
LOC:WindowSize       CSTRING(21)                           ! 
Q:ContextMenu        QUEUE,PRE(Q:CMenu)                    ! 
ID                   LONG                                  ! 
Menu                 CSTRING(128)                          ! 
Action               CSTRING(128)                          ! 
Point                LONG                                  ! 
Xpos                 LONG                                  ! 
Ypos                 LONG                                  ! 
Data_ID              LONG                                  ! 
Data_Pointer         LONG                                  ! 
Brief                CSTRING(1024)                         ! 
StringFEQ            LONG                                  ! 
RegionFEQ            LONG                                  ! 
BoxFEQ               LONG                                  ! 
BGBrief_FEQ          LONG                                  ! 
StartXpos            LONG                                  ! 
StartYpos            LONG                                  ! 
EndXpos              LONG                                  ! 
EndYpos              LONG                                  ! 
TextCoords           LIKE(SA_RECT)                         ! 
Parent_ID            LONG                                  ! 
This_ID              LONG                                  ! 
                     END                                   ! 
CurrentTab           STRING(80)                            ! 
BRW1::View:Browse    VIEW(Invoices)
                       PROJECT(INV:Invoice_Sequence)
                       PROJECT(INV:Name)
                       PROJECT(INV:Date)
                       PROJECT(INV:SubTotal)
                       PROJECT(INV:TAX)
                       PROJECT(INV:Total)
                       PROJECT(INV:Note)
                       PROJECT(INV:Expand)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Bowse_Invoices
INV:Invoice_Sequence   LIKE(INV:Invoice_Sequence)     !List box control field - type derived from field
INV:Name               LIKE(INV:Name)                 !List box control field - type derived from field
INV:Date               LIKE(INV:Date)                 !List box control field - type derived from field
INV:SubTotal           LIKE(INV:SubTotal)             !List box control field - type derived from field
INV:TAX                LIKE(INV:TAX)                  !List box control field - type derived from field
INV:Total              LIKE(INV:Total)                !List box control field - type derived from field
INV:Note               LIKE(INV:Note)                 !List box control field - type derived from field
INV:Expand             LIKE(INV:Expand)               !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Invoices file'),AT(,,725,342),FONT('Microsoft Sans Serif',8,00F5F5F5h,FONT:regular, |
  CHARSET:DEFAULT),RESIZE,MAXIMIZE,CENTER,COLOR(00463B2Ch),GRAY,IMM,MDI,HLP('Invoices'),SYSTEM
                       LIST,AT(3,2,718,114),USE(?Bowse_Invoices),HVSCROLL,FLAT,FORMAT('68R(2)|M~Invoice Sequen' & |
  'ce~C(0)@n-14@80L(2)|M~Name~L(2)@s255@80R(2)|M~Date~C(0)@d6@80D(28)|M~Sub Total~C(0)@' & |
  'N$-17.2@80D(40)|M~TAX~C(0)@N$-17.2@80D(36)|M~Total~C(0)@N$-17.2@80L(2)|M~Note~L(2)@s' & |
  '255@28R(2)|M~Expand~C(0)@n3@'),FROM(Queue:Browse:1),IMM,MSG('Browsing the Invoices file'), |
  TRN
                       BUTTON('&Select'),AT(535,319,49,14),USE(?Select:2),LEFT,ICON('select_32x32.png'),FLAT,MSG('Select the Record'), |
  TIP('Select the Record')
                       BUTTON('&Close'),AT(671,319,49,14),USE(?Close),LEFT,ICON('close_32x32.png'),FLAT,MSG('Close Window'), |
  TIP('Close Window')
                       TEXT,AT(187,319,333,13),USE(?LOG),HIDE
                       BOX,AT(3,143,718,172),USE(?BR_Box),COLOR(COLOR:Black),FILL(COLOR:Black),HIDE,LINEWIDTH(1)
                       BUTTON('&Insert'),AT(2,120,42,12),USE(?Insert)
                       BUTTON('&Change'),AT(45,120,42,12),USE(?Change)
                       BUTTON('&Delete'),AT(86,120,42,12),USE(?Delete)
                       CHECK('Expand All'),AT(667,129),USE(GLO:Inv_Expand_All),HIDE
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Bowse_Invoices
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

LC                  CLASS

Fill_Tree               PROCEDURE(STRING Action)
!Fill_Tree_Clarion       PROCEDURE(STRING Action)
!Fill_Tree_Old           PROCEDURE()
Draw_Invoice_Header     PROCEDURE(LONG X, LONG Y, |
							LONG Cell_Width, LONG Cell_Height, LONG Cell_Round, |
							STRING Label_Text, LONG Label_Padding, STRING Justify_Label, LONG Top_Data_Padding,|
							STRING Data_Text,LONG Data_Padding, STRING Justify_Data, LONG Top_Label_Padding),LONG
Draw_Invoice_Detail_Header      PROCEDURE(LONG X, LONG Y, LONG Cell_Width, LONG Cell_Height, LONG Cell_Round, |
									STRING Label_Text, LONG Label_Padding, STRING Data_Text,LONG Data_Padding),LONG
Draw_Invoice_Detail     PROCEDURE(LONG X, LONG Y, LONG Cell_Width, LONG Cell_Height, LONG Cell_Round, STRING Label_Text, LONG Label_Padding, STRING Data_Text,LONG Data_Padding),LONG
Draw_BG                 PROCEDURE(LONG X, LONG Y, LONG Cell_Width, LONG Cell_Height)

					END

SV                  CLASS()
Log                     PROCEDURE(STRING WhatTolog)
					END



WinGDI              class_photon

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Invoices')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Bowse_Invoices
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Invoices.SetOpenRelated()
  Relate:Invoices.Open()                                   ! File Invoices used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Bowse_Invoices,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Invoices,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  0{PROP:Pixels} = TRUE
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon INV:Invoice_Sequence for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,INV:Invoice_Sequence_Key) ! Add the sort order for INV:Invoice_Sequence_Key for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,INV:Invoice_Sequence,1,BRW1)   ! Initialize the browse locator using  using key: INV:Invoice_Sequence_Key , INV:Invoice_Sequence
  BRW1.AddField(INV:Invoice_Sequence,BRW1.Q.INV:Invoice_Sequence) ! Field INV:Invoice_Sequence is a hot field or requires assignment from browse
  BRW1.AddField(INV:Name,BRW1.Q.INV:Name)                  ! Field INV:Name is a hot field or requires assignment from browse
  BRW1.AddField(INV:Date,BRW1.Q.INV:Date)                  ! Field INV:Date is a hot field or requires assignment from browse
  BRW1.AddField(INV:SubTotal,BRW1.Q.INV:SubTotal)          ! Field INV:SubTotal is a hot field or requires assignment from browse
  BRW1.AddField(INV:TAX,BRW1.Q.INV:TAX)                    ! Field INV:TAX is a hot field or requires assignment from browse
  BRW1.AddField(INV:Total,BRW1.Q.INV:Total)                ! Field INV:Total is a hot field or requires assignment from browse
  BRW1.AddField(INV:Note,BRW1.Q.INV:Note)                  ! Field INV:Note is a hot field or requires assignment from browse
  BRW1.AddField(INV:Expand,BRW1.Q.INV:Expand)              ! Field INV:Expand is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Resize,Resize:SetMinSize)       ! Controls will change size as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  IF ?GLO:Inv_Expand_All{Prop:Checked}
    GLO:Inv_Expand_All = TRUE
  END
  IF NOT ?GLO:Inv_Expand_All{PROP:Checked}
    GLO:Inv_Expand_All = FALSE
  END
  BRW1.AskProcedure = 1                                    ! Will call: Update_Invoices
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Invoices.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    Update_Invoices
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?GLO:Inv_Expand_All
      IF ?GLO:Inv_Expand_All{PROP:Checked}
        GLO:Inv_Expand_All = TRUE
      END
      IF NOT ?GLO:Inv_Expand_All{PROP:Checked}
        GLO:Inv_Expand_All = FALSE
      END
      ThisWindow.Reset()
      LC.Fill_Tree('Reload')
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:CloseWindow
      FREE(Q:Products_List)
    OF EVENT:OpenWindow
      LC.Fill_Tree('Init')
      LC.Fill_Tree('Reload')
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

LC.Fill_Tree        PROCEDURE(STRING Action)

ItemColors              GROUP,PRE(ItemColors)
NF                          ULONG
NB                          ULONG
SF                          ULONG
SB                          ULONG
						END

SNColors                GROUP,PRE(SNColors)
NF                          ULONG
NB                          ULONG
SF                          ULONG
SB                          ULONG
						END

Inv_Columns             GROUP,PRE(Inv_LIST)
Invoice_Number              ULONG
Date                        ULONG
Name                        ULONG
Reference                   ULONG
Valid_Thru                  ULONG
SubTotal                    ULONG
Tax                         ULONG
Total                       ULONG
Currency                    ULONG
MadeBy                      ULONG
						END

Inv_Det_Columns         GROUP,PRE(Inv_DET_LIST)
Seq                         ULONG
Product_Code                ULONG
Quantity                    ULONG
Unit                        ULONG
Item                        ULONG
Purchase_Price              ULONG
Profit                      ULONG
Unit_Price                  ULONG
SubTotal                    ULONG
Currency                    ULONG




						END


GreenBar                BYTE
SerialsCounter          ULONG
SerialsHeader           BYTE
WhatSNLine              ULONG
MainItemPos             ULONG

OtherSNs                ULONG



!Virtual Columns
Padding                 ULONG
Seq                     ULONG
Item                    ULONG
Inv_Qty                 ULONG
Inv_Type                ULONG
Inv_Measurements        ULONG
Inv_Weight              ULONG
Inv_Total_Weight        ULONG
Inv_List_Width          ULONG
Inv_List_Width_Units    ULONG
Inv_Product_Code        ULONG
Inv_Unit                ULONG
Inv_Description         ULONG
Inv_Purchase_Price      ULONG
Inv_Profit              ULONG
Inv_Unit_Price          ULONG
Inv_SubTotal            ULONG
Inv_Currency            ULONG
Inv_Invoice_Number      ULONG
Inv_Date                ULONG
Inv_Name                ULONG
Inv_Reference           ULONG
Inv_Item_Loop           ULONG
Inv_Desc_Loop           ULONG


XX                      ULONG
YY                      ULONG
BXX                     ULONG
BYY                     ULONG

BW                      ULONG
BH                      ULONG

Round_Radius            ULONG
Line_Height             ULONG
Max_List_Lines          ULONG
ThisParent              SIGNED
Current_Line            ULONG
LastFEQ                 LONG
Line_Start              ULONG
TopMarginText           ULONG
Percentile_Units        ULONG
Inv_Header_BG           STRING(128)
Inv_BG                  STRING(128)
Inv_Detail_Header_BG    STRING(128)
Inv_Detail_BG           STRING(128)

Box_Width               ULONG
Box_Height              ULONG
Active_Color            LONG


CODE
    
	0{PROP:Pixels} = TRUE
	0{PROP:Buffer} = 1    
	0{PROP:LazyDisplay} = FALSE
    
	DISPLAY()
    
	FREE(Q:ContextMenu)
	FREE(Q:PolyCoords)
    
   
	CASE Action

	OF 'Init'
		DO Init_ROUTINE
		DO List_Renderer_ROUTINE

	OF 'Reload'
		DO Init_ROUTINE
        !DO Build_View_Pointer_ROUTINE
		DO List_Renderer_ROUTINE
        
	OF 'Resize'
		DO Init_ROUTINE
		DO List_Renderer_ROUTINE
        
	OF 'One_Down'        
		Line_Start += 1
        !DO Init_ROUTINE
		DO List_Renderer_ROUTINE
	OF 'One_Up'
		IF Line_Start = 1 !We cannot go up
            
		ELSE            
			Line_Start -= 1
			DO List_Renderer_ROUTINE
		END
        
	OF 'Page_Down'    
        
		DO List_Measurements_ROUTINE
        
		IF (Line_Start + Max_List_Lines) => RECORDS(Q:View)
			Line_start = RECORDS(Q:View) - Max_List_Lines
			DO List_Renderer_ROUTINE
		END

		IF (Line_Start + Max_List_Lines) =< RECORDS(Q:View)            
			Line_start = Line_start + Max_List_Lines
			DO List_Renderer_ROUTINE
		END
        
!        SV.LOG('Line start = ' & Line_Start)
        
	OF 'Page_Up'
        
		DO List_Measurements_ROUTINE
        
		IF Line_Start - Max_List_Lines => 1
			Line_start = Line_Start - Max_List_Lines
			DO List_Renderer_ROUTINE
		END
        
	OF 'Home'
        
		DO List_Measurements_ROUTINE
		Line_start = 1
		DO List_Renderer_ROUTINE
        
        
	OF 'End'
        
		DO List_Measurements_ROUTINE
		Line_start = RECORDS(Q:View) - Max_List_Lines
		DO List_Renderer_ROUTINE
        
        
	END


	RETURN
    
Init_ROUTINE        ROUTINE

	ThisParent = ?BR_BOX{PROP:Parent}

	IF ?BR_BOX{PROP:Hide} = FALSE
		?BR_BOX{PROP:Hide} = TRUE
	END
	
	LOC:WindowSize = 0{PROP:Width}&0{PROP:Height}

	FREE(Q:View)
	Line_Start = 1
	DO Build_View_Pointer_ROUTINE
!	IF RECORDS(Q:View) = 0        
!		Line_Start = 1
!		DO Build_View_Pointer_ROUTINE     
!	END
!    
	IF Action = 'Reload'
		DO Build_View_Pointer_ROUTINE
	END
    

	ALERT()
	QuickWindow{Prop:Alrt} = MouseRightUp
	QuickWindow{Prop:Alrt} = MouseLeftUp
	QuickWindow{PROP:Alrt} = DownKey
	QuickWindow{PROP:Alrt} = UpKey
	QuickWindow{PROP:Alrt} = HomeKey
	QuickWindow{PROP:Alrt} = EndKey
	QuickWindow{PROP:Alrt} = PgDnKey
	QuickWindow{PROP:Alrt} = PgUpKey
    
	EXIT    

List_Measurements_ROUTINE   ROUTINE
    
	BW = ?BR_BOX{PROP:Width}
	BH = ?BR_BOX{PROP:Height}
	XX = ?BR_BOX{PROP:Xpos}    
	YY = ?BR_BOX{PROP:Ypos}
    
	Line_Height = 36
    
	Line_Height += 1 !No lines separator effect... Trust me on this.

	Max_List_Lines = INT(BH / (Line_Height + 1))    

    !Quotations
	Percentile_Units = BW / 100
    
	Inv_Columns.Invoice_Number                = 6 * Percentile_Units
	Inv_Columns.Invoice_Number              = 6 * Percentile_Units
	Inv_Columns.Date                        = 8 * Percentile_Units
	Inv_Columns.Name                        = 25 * Percentile_Units
	Inv_Columns.Reference                        = 18 * Percentile_Units
	Inv_Columns.Valid_Thru                        = 7 * Percentile_Units
	Inv_Columns.SubTotal                        = 8 * Percentile_Units
	Inv_Columns.Tax                        = 8 * Percentile_Units
	Inv_Columns.Total                        = 8 * Percentile_Units
	Inv_Columns.MadeBy                        = 5 * Percentile_Units

    !Quotations Detail
    
	Inv_Det_Columns.Seq                         = 4 * Percentile_Units
	Inv_Det_Columns.Product_Code                = 10 * Percentile_Units
	Inv_Det_Columns.Quantity                    = 7 * Percentile_Units
	Inv_Det_Columns.Unit                        = 6 * Percentile_Units
	Inv_Det_Columns.Item                        = 35 * Percentile_Units
	Inv_Det_Columns.Purchase_Price              = 9 * Percentile_Units
	Inv_Det_Columns.Profit                      = 9 * Percentile_Units
	Inv_Det_Columns.Unit_Price                  = 9 * Percentile_Units
	Inv_Det_Columns.SubTotal                    = 10 * Percentile_Units
	Inv_Det_Columns.Currency                    = 7 * Percentile_Units
        
	Padding = 1
	LC.Draw_BG(XX,YY,BW,BH)
    

	EXIT
    
Invoice_Header_ROUTINE    ROUTINE
    
	GET(Invoices,Q:V:PTR) !Get the right item
    
	EndX# = LC.Draw_Invoice_Header(XX ,YY + Padding,Inv_Columns.Invoice_Number, Line_Height,3,'Invoice #',0,'Center',1,FORMAT(INV:Invoice_Sequence,@N9),0,'Center',20)
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.Invoice_Number, Line_Height,3,'Invoice #',1,'Center',0,FORMAT(INV:Invoice_Sequence,@N9),0,'Center',20)
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.Date, Line_Height,3,'Date',0,'Center',1,FORMAT(INV:Date,@D6),0,'Center',20)
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.Name, Line_Height,3,'Name',0,'Center',1,CLIP(INV:Name),0,'Center',20)
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.Reference, Line_Height,3,'Reference',0,'Center',1,INV:Note,0,'Center',20)    
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.SubTotal, Line_Height,3,'Sub-Total',-10,'Center',1,FORMAT(INV:SubTotal,@N$12.4),10,'Right',20)
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.Tax, Line_Height,3,'  Tax  ',0,'Center',1,FORMAT(INV:Tax,@N$12.4),10,'Right',20)
	EndX# = LC.Draw_Invoice_Header(XX + EndX# ,YY + Padding,Inv_Columns.Total, Line_Height,3,'  Total  ',0,'Center',1,FORMAT(INV:Total,@N$14.4),10,'Right',20)
      
	EXIT
    

Invoice_Detail_Header_ROUTINE     ROUTINE
        
	EndX# = LC.Draw_Invoice_Detail_Header(XX ,YY + Padding,Inv_Det_Columns.Seq, Line_Height,3,'',0,'Seq#',0)
	EndX# = LC.Draw_Invoice_Detail_Header(XX + EndX# ,YY + Padding,Inv_Det_Columns.Product_Code, Line_Height,3,'',0,'PRODUCT CODE',0)
	EndX# = LC.Draw_Invoice_Detail_Header(XX + EndX# ,YY + Padding,Inv_Det_Columns.Quantity, Line_Height,3,'',0,'Quantity',0)
	EndX# = LC.Draw_Invoice_Detail_Header(XX + EndX# ,YY + Padding,Inv_Det_Columns.Item, Line_Height,3,'',0,'Item',0)
	EndX# = LC.Draw_Invoice_Detail_Header(XX + EndX# ,YY + Padding,Inv_Det_Columns.Unit_Price, Line_Height,3,'',0,'Unit Price',0)
	EndX# = LC.Draw_Invoice_Detail_Header(XX + EndX# ,YY + Padding,Inv_Det_Columns.SubTotal, Line_Height,3,'',0,'Subtotal',0)
	
	EXIT

    
Invoice_Detail_ROUTINE    ROUTINE
    
	GET(Invoice_Detail,Q:V:PTR)
    
	EndX# = LC.Draw_Invoice_Detail(XX ,YY + Padding,Inv_Det_Columns.Seq, Line_Height,3,'',0,FORMAT(INDET:Invoice_Detail_Sequence,@P####P),0)
	EndX# = LC.Draw_Invoice_Detail(XX + EndX# ,YY + Padding,Inv_Det_Columns.Product_Code, Line_Height,3,'',0,INDET:Item_Code,0)
	EndX# = LC.Draw_Invoice_Detail(XX + EndX# ,YY + Padding,Inv_Det_Columns.Quantity, Line_Height,3,'',0,FORMAT(INDET:Quantity,@N10.2),0)
	EndX# = LC.Draw_Invoice_Detail(XX + EndX# ,YY + Padding,Inv_Det_Columns.Item, Line_Height,3,'',0,CLIP(INDET:Item),0)
	EndX# = LC.Draw_Invoice_Detail(XX + EndX# ,YY + Padding,Inv_Det_Columns.Unit_Price, Line_Height,3,'',0,FORMAT(INDET:Price,@N$12.4),0)
	EndX# = LC.Draw_Invoice_Detail(XX + EndX# ,YY + Padding,Inv_Det_Columns.SubTotal, Line_Height,3,'',0,FORMAT(INDET:Extended,@N$14.4),0)
    
    
	EXIT
    
    
Destroy_Items_ROUTINE       ROUTINE
    
	GET(Q:Browse,1)
    
	FirstFEQ# = Q:BR:FEQ
	GET(Q:Browse,RECORDS(Q:Browse))
        
	LastFEQ# = Q:BR:FEQ
    
    
	DESTROY(FirstFEQ#,LastFEQ#)
       
	EXIT
    

    
List_Renderer_ROUTINE       ROUTINE
    
	DO Destroy_Items_ROUTINE !Let's clear the list... Text, icons, buttons.... 
    
	DO List_Measurements_ROUTINE
    
	ThisLineLoop# = Line_Start
    
	SV.LOG('Starting List_Renderer_ROUTINE')    
	SV.LOG('Line start = '& Line_Start)

	GET(Q:View,-1)
    
    
	ThisStep# = 0

	LOOP Max_List_Lines TIMES
        
		ThisStep# += 1

		Q:V:Seq = ThisLineLoop#
		GET(Q:View,Q:V:Seq)

		CASE Q:V:Kind

		OF 'INV_HEADER'
			DO Invoice_Header_ROUTINE
		OF 'INV'
			DO Invoice_Header_ROUTINE
		OF 'INV_DET_HEADER'
			DO Invoice_Detail_Header_ROUTINE
		OF 'INV_DET'
			DO Invoice_Detail_ROUTINE         
		END !CASE
        
		XX = ?BR_BOX{PROP:Xpos}
		YY = ?BR_BOX{PROP:Ypos} + (Line_Height * ThisStep#) + (1 * ThisStep#)

		ThisLineLoop# += 1

	END !LOOP
    
    
	EXIT

    
Build_View_Pointer_ROUTINE  ROUTINE

!    ThisLoopInnited# = FALSE
	FREE(Q:View)    
	CLEAR(INV:RECORD)
	SET(INV:Invoice_Sequence_Key)
	LOOP WHILE ACCESS:Invoices.Next() = LEVEL:Benign

        
		Q:V:Seq = RECORDS(Q:View) + 1
		Q:V:Lines = 1
		Q:V:Kind = 'INV'
		Q:V:PTR = POINTER(Invoices)
		ADD(Q:View)
      
        
		IF INV:Expand = TRUE OR GLO:Inv_Expand_All = TRUE
			Start_Detail# = 0
			CLEAR(INDET:RECORD)
			INDET:Invoice_Sequence = INV:Invoice_Sequence
			SET(INDET:Invoice_Detail_Sequence_Key,INDET:Invoice_Detail_Sequence_Key)
        
			LOOP WHILE Access:Invoice_Detail.Next() = LEVEL:Benign      
				IF INDET:Invoice_Sequence <> INV:Invoice_Sequence THEN BREAK.            
				IF Start_detail# = 0
					Q:V:Seq = RECORDS(Q:View) + 1
					Q:V:Lines = 1
					Q:V:Kind = 'INV_DET_HEADER'
					Q:V:PTR = 0
					ADD(Q:View)
				END
            
				IF LEN(INDET:Description) => 1
					Q:V:Lines = 2
				ELSE
					Q:V:Lines = 1
				END            
                
				Q:V:Seq = RECORDS(Q:View) + 1
				Q:V:Kind = 'INV_DET'
				Q:V:PTR = POINTER(Invoice_Detail)
				ADD(Q:View)
				Start_detail# = 1
			END !LOOP
		END !IF
        
	END !LOOP
    
	ThisFLoop# = 1
	ThisFLines# = 0
    
    
    
	LOOP RECORDS(Q:View) TIMES
		GET(Q:View,ThisFLoop#)
		ThisFLoop# += 1
		ThisFLines# += Q:V:Lines
	END
    
    
    !SV.LOG('Records on VIEW Pointers = ' & RECORDS(Q:View) & ' With ' & ThisFLines# )
    
	EXIT
LC.Draw_BG          PROCEDURE(LONG X, LONG Y, LONG Cell_Width, LONG Cell_Height)

TextCoords              LIKE(SA_RECT)
SizeStruct              LIKE(SA_SIZE)
PointsSTR               STRING(8192)
InitialRadius           LONG
MaxInitialItems         uLONG
ExtraItems              uLONG
MinDistance             uLONG
Q_ID                    LONG
BG_Hor_Padding          uLONG
BG_Ver_Padding          uLONG
TextOut                 CSTRING(128)
hFont                   uLONG
hTmp                    uLONG
FontFace                CSTRING(128)
ThisTextPoints          uLONG
ThisTextHeight          uLONG
ThisTextLen             uLONG
ThisTextWidth           uLONG
ThisTextXpos            uLONG
ThisTextYpos            uLONG
ThisRegionStartX        uLONG
ThisRegionStartY        uLONG
ThisRegionEndX          uLONG
ThisRegionEndY          uLONG
Multiplier              uLONG
IconName                CSTRING(256)
hBitmap                 uLONG
hdcorg                  uLONG
hdcTemp                 uLONG
IconHandle              uLONG
W_Paint_hdc             uLONG
PStructure              LIKE(SA_PAINTSTRUCT)
PStructure_ADD          uLONG
hWndRgn                 uLONG
UniWinRect              LIKE(SA_RECT)
LocBlackBrush           uLONG
RoundedCornerVal        ULONG
TextPadding             uLONG
ThisParent              LONG
LastFEQ                 LONG
TopMarginText           ULONG
uAlignPrev              ULONG
RandomColor             LONG





CODE
    
    
    WinGDI.Allocate(0) !0 Means the client window...
		
    WinGDI.PenColor(005E5242h)
    SetDCBrushResult# = WinGDI.BrushColor(005E5242h)
    RoundRectResult# = WinGDI.RoundRect(X,Y,X + Cell_Width,Y + Cell_Height,0)

    
    WinGDI.FreeGDIResources()
    

    RETURN
    
    
LC.Draw_Invoice_Header        PROCEDURE(LONG X, LONG Y, |
                                LONG Cell_Width, LONG Cell_Height, LONG Cell_Round, |
                                STRING Label_Text, LONG Label_Padding, STRING Justify_Label, LONG Top_Label_Padding,|
                                STRING Data_Text,LONG Data_Padding, STRING Justify_Data, LONG Top_Data_Padding)

TextCoords                      LIKE(SA_RECT)
SizeStruct                      LIKE(SA_SIZE)
PointsSTR                       STRING(8192)
InitialRadius                   LONG
MaxInitialItems                 uLONG
ExtraItems                      uLONG
MinDistance                     uLONG
Q_ID                            LONG
BG_Hor_Padding                  uLONG
BG_Ver_Padding                  uLONG
TextOut                         CSTRING(128)
hFont                           uLONG
hTmp                            uLONG
FontFace                        CSTRING(128)
ThisTextPoints                  uLONG
ThisTextHeight                  uLONG
ThisTextLen                     uLONG
ThisTextWidth                   uLONG
ThisTextXpos                    uLONG
ThisTextYpos                    uLONG
ThisRegionStartX                uLONG
ThisRegionStartY                uLONG
ThisRegionEndX                  uLONG
ThisRegionEndY                  uLONG
Multiplier                      uLONG
IconName                        CSTRING(256)
hBitmap                         uLONG
hdcorg                          uLONG
hdcTemp                         uLONG
IconHandle                      uLONG
W_Paint_hdc                     uLONG
PStructure                      LIKE(SA_PAINTSTRUCT)
PStructure_ADD                  uLONG
hWndRgn                         uLONG
UniWinRect                      LIKE(SA_RECT)
LocBlackBrush                   uLONG
RoundedCornerVal                ULONG
TextPadding                     uLONG
ThisParent                      LONG
LastFEQ                         LONG
TopMarginText                   ULONG
uAlignPrev                      ULONG





CODE
    
    
    WinGDI.Allocate(0) !0 Means the client window...
    
    
    !Draw main cell
    WinGDI.PenColor(003E3120h)
    SetDCBrushResult# = WinGDI.BrushColor(003E3120h)
    RoundRectResult# = WinGDI.RoundRect(X,Y,X + Cell_Width,Y + Cell_Height,Cell_Round)
    
    
    !Draw secondary cell
    
    WinGDI.PenColor(003E3120h)
    SetDCBrushResult# = WinGDI.BrushColor(COLOR:Green)
    RoundRectResult# = WinGDI.RoundRect(X + 1,Y,X + Cell_Width - 2,Y + 14,7)
        
    !Common Text Settings
    
    TextPadding = 1.035
    ThisTextPoints = 14
    FontFace = 'JetBrains Mono Medium'    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    TextOut = Data_Text
    ThisTextLen = LEN(TextOut)
    SA_GetTextExtentPoint32(WinGDI.controlDC,TextOut,ThisTextLen,SizeStruct)
    WinGDI.RemoveFontResourceGDI()
    
    CASE UPPER(Justify_Data)

    OF 'CENTER'
        
        ThisTextXpos = X + Data_Padding
        ThisTextYpos = Y + Top_Data_Padding    
        ThisTextWidth = SizeStruct.cx
        ThisTextHeight = SizeStruct.cy    
        TextCoords.left = ThisTextXpos + ((Cell_Width - ThisTextWidth) / 2)
        TextCoords.right = TextCoords.left + ThisTextWidth! * 1.4
        TextCoords.top = ThisTextYpos! + ((Cell_Height - ThisTextHeight) / 2)
        TextCoords.bottom = TextCoords.top + ThisTextHeight


    OF 'LEFT'
        
        ThisTextXpos = X + Data_Padding
        ThisTextYpos = Y + Top_Data_Padding
        ThisTextWidth = SizeStruct.cx
        ThisTextHeight = SizeStruct.cy    
        TextCoords.left = ThisTextXpos !+ ThisTextWidth
        TextCoords.right = TextCoords.left + ThisTextWidth! * 1.4
        TextCoords.top = ThisTextYpos! + ((Cell_Height - ThisTextHeight) / 2)
        TextCoords.bottom = TextCoords.top + ThisTextHeight

    OF 'RIGHT'
        
        ThisTextXpos = X + Cell_Width - Data_Padding - SizeStruct.cx
        ThisTextYpos = Y + Top_Data_Padding
        ThisTextWidth = SizeStruct.cx
        ThisTextHeight = SizeStruct.cy    
        TextCoords.left = ThisTextXpos !+ ThisTextWidth
        TextCoords.right = TextCoords.left + ThisTextWidth! * 1.4
        TextCoords.top = ThisTextYpos! + ((Cell_Height - ThisTextHeight) / 2)
        TextCoords.bottom = TextCoords.top + ThisTextHeight

    END
    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    WinGDI.FontColor(COLOR:White)
    WinGDI.Background(COLOR:Red)
    SetDCBrushResult# = WinGDI.BrushColor(00795100h)    
    TextOut = Data_Text
    SA_DrawText(WinGDI.controlDC,TextOut,ThisTextLen,TextCoords,0)
    WinGDI.RemoveFontResourceGDI()
    

    !Common Text Settings
    
    TextPadding = 1.035
    ThisTextPoints = 12
    FontFace = 'EngraversGothic BT Regular'    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    TextOut = Label_Text
    ThisTextLen = LEN(TextOut)
    SA_GetTextExtentPoint32(WinGDI.controlDC,TextOut,ThisTextLen,SizeStruct)
    WinGDI.RemoveFontResourceGDI()
    
    CASE UPPER(Justify_Label)

    OF 'CENTER'
        
        ThisTextXpos = X + Data_Padding
        ThisTextYpos = Y + Top_Label_Padding
        ThisTextWidth = SizeStruct.cx
        ThisTextHeight = SizeStruct.cy    
        TextCoords.left = ThisTextXpos + ((Cell_Width - ThisTextWidth) / 2)
        TextCoords.right = TextCoords.left + ThisTextWidth
        TextCoords.top = ThisTextYpos
        TextCoords.bottom = TextCoords.top + ThisTextHeight

    OF 'LEFT'
        
        ThisTextXpos = X + Data_Padding
        ThisTextYpos = Y + Top_Label_Padding
        ThisTextWidth = SizeStruct.cx
        ThisTextHeight = SizeStruct.cy    
        TextCoords.left = ThisTextXpos 
        TextCoords.right = TextCoords.left + ThisTextWidth
        TextCoords.top = ThisTextYpos
        TextCoords.bottom = TextCoords.top + ThisTextHeight

    OF 'RIGHT'
        
        ThisTextXpos = X + Cell_Width - Data_Padding - SizeStruct.cx
        ThisTextYpos = Y + Top_Label_Padding
        ThisTextWidth = SizeStruct.cx
        ThisTextHeight = SizeStruct.cy    
        TextCoords.left = ThisTextXpos 
        TextCoords.right = TextCoords.left + ThisTextWidth
        TextCoords.top = ThisTextYpos
        TextCoords.bottom = TextCoords.top + ThisTextHeight

    END
    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    WinGDI.FontColor(COLOR:White)
    WinGDI.Background(COLOR:Red)
    SetDCBrushResult# = WinGDI.BrushColor(Color:Green)    
    TextOut = Label_Text
    SA_DrawText(WinGDI.controlDC,TextOut,ThisTextLen,TextCoords,0)
    WinGDI.RemoveFontResourceGDI()
    
    WinGDI.FreeGDIResources()
    

    RETURN(X + Cell_Width)
    
LC.Draw_Invoice_Detail_Header PROCEDURE(LONG X, LONG Y, LONG Cell_Width, LONG Cell_Height, LONG Cell_Round, STRING Label_Text, LONG Label_Padding, STRING Data_Text,LONG Data_Padding)

TextCoords                      LIKE(SA_RECT)
SizeStruct                      LIKE(SA_SIZE)
PointsSTR                       STRING(8192)
InitialRadius                   LONG
MaxInitialItems                 uLONG
ExtraItems                      uLONG
MinDistance                     uLONG
Q_ID                            LONG
BG_Hor_Padding                  uLONG
BG_Ver_Padding                  uLONG
TextOut                         CSTRING(128)
hFont                           uLONG
hTmp                            uLONG
FontFace                        CSTRING(128)
ThisTextPoints                  uLONG
ThisTextHeight                  uLONG
ThisTextLen                     uLONG
ThisTextWidth                   uLONG
ThisTextXpos                    uLONG
ThisTextYpos                    uLONG
ThisRegionStartX                uLONG
ThisRegionStartY                uLONG
ThisRegionEndX                  uLONG
ThisRegionEndY                  uLONG
Multiplier                      uLONG
IconName                        CSTRING(256)
hBitmap                         uLONG
hdcorg                          uLONG
hdcTemp                         uLONG
IconHandle                      uLONG
W_Paint_hdc                     uLONG
PStructure                      LIKE(SA_PAINTSTRUCT)
PStructure_ADD                  uLONG
hWndRgn                         uLONG
UniWinRect                      LIKE(SA_RECT)
LocBlackBrush                   uLONG
RoundedCornerVal                ULONG
TextPadding                     uLONG
ThisParent                      LONG
LastFEQ                         LONG
TopMarginText                   ULONG
uAlignPrev                      ULONG





CODE
    
    
    
    ThisTextPoints = 10
    FontFace = 'De Valencia Regular'
	
    WinGDI.Allocate(0) !0 Means the client window...
		
    WinGDI.PenColor(003E3120h)
    SetDCBrushResult# = WinGDI.BrushColor(003E3120h)
    RoundRectResult# = WinGDI.RoundRect(X,Y,X + Cell_Width,Y + Cell_Height,Cell_Round)

    TextOut = UPPER(Label_Text)
    ThisTextLen = LEN(TextOut)
    SA_GetTextExtentPoint32(WinGDI.controlDC,TextOut,ThisTextLen,SizeStruct)
    WinGDI.RemoveFontResourceGDI()
    
    TextPadding = 1.035

    
    ThisTextHeight = SizeStruct.cy 
    ThisTextWidth = INT(SizeStruct.cx * TextPadding) !Extra Pixels, trust me on this

    

    ThisTextHeight = INT(SizeStruct.cy * 1.3) !Height, trust me also on this
    ThisTextXpos = X
    ThisTextYpos = Y
    TextCoords.left = ThisTextXpos + ((Cell_Width - ThisTextWidth ) /2)
    TextCoords.top = ThisTextYpos !+ ((Cell_Height - ThisTextHeight) / 2)

    
    IF ThisTextLen < 7
        TextCoords.right = TextCoords.left + Cell_Width
    ELSE
        
        TextCoords.right = TextCoords.left + ThisTextWidth
    END
    
    TextCoords.bottom = ThisTextYpos + ThisTextHeight
	
    
    WinGDI.PenColor(00795100h)
    SetDCBrushResult# = WinGDI.BrushColor(00795100h)
    
    
!    RoundRectResult# = WinGDI.RoundRect(X +  12 ,Y, X + Cell_Width - 20,Y + 10,Cell_Round)

    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    WinGDI.FontColor(COLOR:Green)

    WinGDI.Background(COLOR:Red)

    !SA_DrawText(WinGDI.controlDC,TextOut,ThisTextLen,TextCoords,0) 
    
    
    ThisTextPoints = 14
    TextOut = UPPER(Data_Text)
    ThisTextLen = LEN(UPPER(TextOut))
    SA_GetTextExtentPoint32(WinGDI.controlDC,TextOut,ThisTextLen,SizeStruct)
    WinGDI.RemoveFontResourceGDI()
    
    TextPadding = 1.035
    ThisTextXpos = X + Data_Padding
    ThisTextYpos = Y 
    
    ThisTextWidth = SizeStruct.cx 
    ThisTextHeight = SizeStruct.cy * 1.3
    
    TextCoords.left = ThisTextXpos + ((Cell_Width - (ThisTextWidth * 1.4) ) /2)

    TextCoords.right = TextCoords.left + ThisTextWidth * 1.4
    
    
    TextCoords.top = ThisTextYpos + ((Cell_Height - ThisTextHeight) / 2)
    TextCoords.bottom = TextCoords.top + ThisTextHeight
    

    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    WinGDI.FontColor(00C2D6C2h)    

    SA_DrawText(WinGDI.controlDC,TextOut,ThisTextLen,TextCoords,0) 
    WinGDI.RemoveFontResourceGDI()
    
    
    WinGDI.FreeGDIResources()
    

    RETURN(X + Cell_Width)
    
LC.Draw_Invoice_Detail PROCEDURE(LONG X, LONG Y, LONG Cell_Width, LONG Cell_Height, LONG Cell_Round, STRING Label_Text, LONG Label_Padding, STRING Data_Text,LONG Data_Padding)

TextCoords                      LIKE(SA_RECT)
SizeStruct                      LIKE(SA_SIZE)
PointsSTR                       STRING(8192)
InitialRadius                   LONG
MaxInitialItems                 uLONG
ExtraItems                      uLONG
MinDistance                     uLONG
Q_ID                            LONG
BG_Hor_Padding                  uLONG
BG_Ver_Padding                  uLONG
TextOut                         CSTRING(128)
hFont                           uLONG
hTmp                            uLONG
FontFace                        CSTRING(128)
ThisTextPoints                  uLONG
ThisTextHeight                  uLONG
ThisTextLen                     uLONG
ThisTextWidth                   uLONG
ThisTextXpos                    uLONG
ThisTextYpos                    uLONG
ThisRegionStartX                uLONG
ThisRegionStartY                uLONG
ThisRegionEndX                  uLONG
ThisRegionEndY                  uLONG
Multiplier                      uLONG
IconName                        CSTRING(256)
hBitmap                         uLONG
hdcorg                          uLONG
hdcTemp                         uLONG
IconHandle                      uLONG
W_Paint_hdc                     uLONG
PStructure                      LIKE(SA_PAINTSTRUCT)
PStructure_ADD                  uLONG
hWndRgn                         uLONG
UniWinRect                      LIKE(SA_RECT)
LocBlackBrush                   uLONG
RoundedCornerVal                ULONG
TextPadding                     uLONG
ThisParent                      LONG
LastFEQ                         LONG
TopMarginText                   ULONG
uAlignPrev                      ULONG





CODE
    
    
    
    ThisTextPoints = 10
    FontFace = 'JetBrains Mono Medium'
	
    WinGDI.Allocate(0) !0 Means the client window...
		
    WinGDI.PenColor(003E3120h)
    SetDCBrushResult# = WinGDI.BrushColor(003E3120h)
    RoundRectResult# = WinGDI.RoundRect(X,Y,X + Cell_Width,Y + Cell_Height,Cell_Round)

    TextOut = UPPER(Label_Text)
    ThisTextLen = LEN(TextOut)
    SA_GetTextExtentPoint32(WinGDI.controlDC,TextOut,ThisTextLen,SizeStruct)
    WinGDI.RemoveFontResourceGDI()
    
    TextPadding = 1.035

    
    ThisTextHeight = SizeStruct.cy 
    ThisTextWidth = INT(SizeStruct.cx * TextPadding) !Extra Pixels, trust me on this

    

    ThisTextHeight = INT(SizeStruct.cy * 1.3) !Height, trust me also on this
    ThisTextXpos = X
    ThisTextYpos = Y
    TextCoords.left = ThisTextXpos + ((Cell_Width - ThisTextWidth ) /2)
    TextCoords.top = ThisTextYpos !+ ((Cell_Height - ThisTextHeight) / 2)

    
    IF ThisTextLen < 7
        TextCoords.right = TextCoords.left + Cell_Width
    ELSE
        
        TextCoords.right = TextCoords.left + ThisTextWidth
    END
    
    TextCoords.bottom = ThisTextYpos + ThisTextHeight
	
    
    WinGDI.PenColor(00795100h)
    SetDCBrushResult# = WinGDI.BrushColor(00795100h)
    
    
!    RoundRectResult# = WinGDI.RoundRect(X +  12 ,Y, X + Cell_Width - 20,Y + 10,Cell_Round)

    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    !WinGDI.FontColor(00DCCEBCh)
    WinGDI.FontColor(00FFFFE0h)

    WinGDI.Background(COLOR:Red)

    !SA_DrawText(WinGDI.controlDC,TextOut,ThisTextLen,TextCoords,0) 
    
    
    ThisTextPoints = 14
    TextOut = UPPER(Data_Text)
    ThisTextLen = LEN(UPPER(TextOut))
    SA_GetTextExtentPoint32(WinGDI.controlDC,TextOut,ThisTextLen,SizeStruct)
    WinGDI.RemoveFontResourceGDI()
    TextPadding = 1.035
    ThisTextXpos = X + Data_Padding
    ThisTextYpos = Y 
    
    ThisTextWidth = SizeStruct.cx 
    ThisTextHeight = SizeStruct.cy * 1.3
    
    TextCoords.left = ThisTextXpos + ((Cell_Width - (ThisTextWidth * 1.4) ) /2)

    TextCoords.right = TextCoords.left + ThisTextWidth * 1.4
    
    
    TextCoords.top = ThisTextYpos + ((Cell_Height - ThisTextHeight) / 2)
    TextCoords.bottom = TextCoords.top + ThisTextHeight
    

    
    WinGDI.InitFont(FontFace,ThisTextPoints)
    WinGDI.FontColor(00FFFFE0h)    

    SA_DrawText(WinGDI.controlDC,TextOut,ThisTextLen,TextCoords,0) 
    WinGDI.RemoveFontResourceGDI()
    
    
    WinGDI.FreeGDIResources()
    

    RETURN(X + Cell_Width)
    
SV.Log              PROCEDURE(STRING WhatToLog)

CODE
	
	?Log{PROP:Text} = CLIP(WhatToLog) & '<13,10>' & ?Log{PROP:Text}
	DISPLAY(?Log)
	
	
	RETURN

BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select:2
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Form Invoices
!!! </summary>
Update_Invoices PROCEDURE 

CurrentTab           STRING(80)                            ! 
ActionMessage        CSTRING(40)                           ! 
BRW6::View:Browse    VIEW(Invoice_Detail)
                       PROJECT(INDET:Item_Code)
                       PROJECT(INDET:Item)
                       PROJECT(INDET:Quantity)
                       PROJECT(INDET:Price)
                       PROJECT(INDET:Extended)
                       PROJECT(INDET:Invoice_Detail_Sequence)
                       PROJECT(INDET:Invoice_Sequence)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
INDET:Item_Code        LIKE(INDET:Item_Code)          !List box control field - type derived from field
INDET:Item             LIKE(INDET:Item)               !List box control field - type derived from field
INDET:Quantity         LIKE(INDET:Quantity)           !List box control field - type derived from field
INDET:Price            LIKE(INDET:Price)              !List box control field - type derived from field
INDET:Extended         LIKE(INDET:Extended)           !List box control field - type derived from field
INDET:Invoice_Detail_Sequence LIKE(INDET:Invoice_Detail_Sequence) !Primary key field - type derived from field
INDET:Invoice_Sequence LIKE(INDET:Invoice_Sequence)   !Browse key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::INV:Record  LIKE(INV:RECORD),THREAD
QuickWindow          WINDOW('Form Invoices'),AT(,,659,290),FONT('Microsoft Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  RESIZE,CENTER,GRAY,IMM,MDI,HLP('Update_Invoices'),SYSTEM
                       BUTTON('&OK'),AT(547,270,49,14),USE(?OK),LEFT,ICON('ok_32x32.png'),DEFAULT,FLAT,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON('&Cancel'),AT(601,270,49,14),USE(?Cancel),LEFT,ICON('cancel_32x32.png'),FLAT,MSG('Cancel operation'), |
  TIP('Cancel operation')
                       PROMPT('Name:'),AT(11,17),USE(?INV:Name:Prompt),TRN
                       ENTRY(@s255),AT(42,17,270,10),USE(INV:Name)
                       PROMPT('Date:'),AT(11,2),USE(?INV:Date:Prompt),TRN
                       ENTRY(@d6),AT(42,2,104,10),USE(INV:Date)
                       PROMPT('Sub Total:'),AT(502,220),USE(?INV:SubTotal:Prompt),TRN
                       ENTRY(@N$-17.2),AT(545,220,104,10),USE(INV:SubTotal),DECIMAL(12),READONLY
                       PROMPT('TAX:'),AT(518,234),USE(?INV:TAX:Prompt),TRN
                       ENTRY(@N$-17.2),AT(545,234,104,10),USE(INV:TAX),DECIMAL(12),READONLY
                       PROMPT('Total:'),AT(517,248),USE(?INV:Total:Prompt),TRN
                       ENTRY(@N$-17.2),AT(545,248,104,10),USE(INV:Total),DECIMAL(12),READONLY
                       PROMPT('Note:'),AT(325,18),USE(?INV:Note:Prompt),TRN
                       ENTRY(@s255),AT(356,18,270,10),USE(INV:Note)
                       LIST,AT(12,36,637,180),USE(?List),FORMAT('128L(2)|M~Item Code~L(0)@s32@300L(2)|M~Item~L' & |
  '(0)@s255@60L(2)|M~Quantity~D(12)@n-14.2@64L(2)|M~Price~D(12)@n$-14.2@76L(2)|M~Extend' & |
  'ed~D(12)@n$-17.2@'),FROM(Queue:Browse),IMM
                       BUTTON('&Insert'),AT(11,218,42,12),USE(?Insert)
                       BUTTON('&Change'),AT(54,218,42,12),USE(?Change)
                       BUTTON('&Delete'),AT(95,218,42,12),USE(?Delete)
                       CHECK('Expand:'),AT(11,274),USE(INV:Expand)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

BRW6                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW6::Sort0:Locator  StepLocatorClass                      ! Default Locator
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Record Will Be Added'
  OF ChangeRecord
    ActionMessage = 'Record Will Be Changed'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Update_Invoices')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?OK
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(INV:Record,History::INV:Record)
  SELF.AddHistoryField(?INV:Name,2)
  SELF.AddHistoryField(?INV:Date,3)
  SELF.AddHistoryField(?INV:SubTotal,4)
  SELF.AddHistoryField(?INV:TAX,5)
  SELF.AddHistoryField(?INV:Total,6)
  SELF.AddHistoryField(?INV:Note,7)
  SELF.AddHistoryField(?INV:Expand,8)
  SELF.AddUpdateFile(Access:Invoices)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Invoice_Detail.SetOpenRelated()
  Relate:Invoice_Detail.Open()                             ! File Invoice_Detail used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Invoices
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.CancelAction = Cancel:Cancel+Cancel:Query         ! Confirm cancel
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW6.Init(?List,Queue:Browse.ViewPosition,BRW6::View:Browse,Queue:Browse,Relate:Invoice_Detail,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  IF SELF.Request = ViewRecord                             ! Configure controls for View Only mode
    ?INV:Name{PROP:ReadOnly} = True
    ?INV:Date{PROP:ReadOnly} = True
    ?INV:SubTotal{PROP:ReadOnly} = True
    ?INV:TAX{PROP:ReadOnly} = True
    ?INV:Total{PROP:ReadOnly} = True
    ?INV:Note{PROP:ReadOnly} = True
    DISABLE(?Insert)
    DISABLE(?Change)
    DISABLE(?Delete)
  END
  Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)      ! Controls like list boxes will resize, whilst controls like buttons will move
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW6.Q &= Queue:Browse
  BRW6.AddSortOrder(,INDET:Invoice_Sequence_Key)           ! Add the sort order for INDET:Invoice_Sequence_Key for sort order 1
  BRW6.AddRange(INDET:Invoice_Sequence,Relate:Invoice_Detail,Relate:Invoices) ! Add file relationship range limit for sort order 1
  BRW6.AddLocator(BRW6::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW6::Sort0:Locator.Init(,INDET:Invoice_Sequence,1,BRW6) ! Initialize the browse locator using  using key: INDET:Invoice_Sequence_Key , INDET:Invoice_Sequence
  BRW6.AddField(INDET:Item_Code,BRW6.Q.INDET:Item_Code)    ! Field INDET:Item_Code is a hot field or requires assignment from browse
  BRW6.AddField(INDET:Item,BRW6.Q.INDET:Item)              ! Field INDET:Item is a hot field or requires assignment from browse
  BRW6.AddField(INDET:Quantity,BRW6.Q.INDET:Quantity)      ! Field INDET:Quantity is a hot field or requires assignment from browse
  BRW6.AddField(INDET:Price,BRW6.Q.INDET:Price)            ! Field INDET:Price is a hot field or requires assignment from browse
  BRW6.AddField(INDET:Extended,BRW6.Q.INDET:Extended)      ! Field INDET:Extended is a hot field or requires assignment from browse
  BRW6.AddField(INDET:Invoice_Detail_Sequence,BRW6.Q.INDET:Invoice_Detail_Sequence) ! Field INDET:Invoice_Detail_Sequence is a hot field or requires assignment from browse
  BRW6.AddField(INDET:Invoice_Sequence,BRW6.Q.INDET:Invoice_Sequence) ! Field INDET:Invoice_Sequence is a hot field or requires assignment from browse
  BRW6.AskProcedure = 1                                    ! Will call: Update_Invoice_Detail
  BRW6.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Invoice_Detail.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.PrimeFields PROCEDURE

  CODE
  INV:Date = TODAY()
  PARENT.PrimeFields


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    Update_Invoice_Detail
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window


BRW6.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

