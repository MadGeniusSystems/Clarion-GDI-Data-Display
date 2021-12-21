  MEMBER('GDI_Browse.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
GDI_Browse_BC0:DctInit    PROCEDURE()
GDI_Browse_BC0:DctKill    PROCEDURE()
GDI_Browse_BC0:FilesInit  PROCEDURE()
  END

Hide:Access:Invoices CLASS(FileManager),TYPE               ! FileManager for Invoices
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Invoices CLASS(RelationManager),TYPE           ! RelationManager for Invoices
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Invoice_Detail CLASS(FileManager),TYPE         ! FileManager for Invoice_Detail
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Invoice_Detail CLASS(RelationManager),TYPE     ! RelationManager for Invoice_Detail
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:Invoices &Hide:Access:Invoices,AUTO,THREAD
_Hide:Relate:Invoices &Hide:Relate:Invoices,AUTO,THREAD
_Hide:Access:Invoice_Detail &Hide:Access:Invoice_Detail,AUTO,THREAD
_Hide:Relate:Invoice_Detail &Hide:Relate:Invoice_Detail,AUTO,THREAD


GDI_Browse_BC0:DctInit PROCEDURE()
  CODE
  _Hide:Access:Invoices &= NEW(Hide:Access:Invoices)
  _Hide:Relate:Invoices &= NEW(Hide:Relate:Invoices)
  _Hide:Access:Invoice_Detail &= NEW(Hide:Access:Invoice_Detail)
  _Hide:Relate:Invoice_Detail &= NEW(Hide:Relate:Invoice_Detail)
  Relate:Invoices &= _Hide:Relate:Invoices
  Relate:Invoice_Detail &= _Hide:Relate:Invoice_Detail


GDI_Browse_BC0:FilesInit PROCEDURE()
  CODE
  _Hide:Relate:Invoices.Init
  _Hide:Relate:Invoice_Detail.Init


GDI_Browse_BC0:DctKill PROCEDURE()
  CODE
  _Hide:Relate:Invoices.Kill
  DISPOSE(_Hide:Relate:Invoices)
  _Hide:Relate:Invoice_Detail.Kill
  DISPOSE(_Hide:Relate:Invoice_Detail)


Hide:Relate:Invoices.Init PROCEDURE()
  CODE
  _Hide:Access:Invoices.Init
  SELF.Init(Access:Invoices,1)


Hide:Access:Invoices.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= INV:Record
  SELF.FileNameValue = 'Invoices'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Invoices
  PARENT.Init
  Access:Invoices &= SELF


Hide:Access:Invoices.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Invoices &= NULL                                  ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Invoices.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Invoices,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(INV:Invoice_Sequence_Key,'INV:Invoice_Sequence_Key',1)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Invoices.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Invoice_Detail,RI:CASCADE,RI:RESTRICT,INDET:Invoice_Sequence_Key)
  SELF.AddRelationLink(INV:Invoice_Sequence,INDET:Invoice_Sequence)


Hide:Relate:Invoices.Kill PROCEDURE

  CODE
  _Hide:Access:Invoices.Kill                               ! Kill the file manager
  PARENT.Kill
  Relate:Invoices &= NULL                                  ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Invoices)                           ! destroy the file manager


Hide:Relate:Invoice_Detail.Init PROCEDURE()
  CODE
  _Hide:Access:Invoice_Detail.Init
  SELF.Init(Access:Invoice_Detail,1)


Hide:Access:Invoice_Detail.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= INDET:Record
  SELF.FileNameValue = 'Invoice_Detail'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Invoice_Detail
  PARENT.Init
  Access:Invoice_Detail &= SELF


Hide:Access:Invoice_Detail.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Invoice_Detail &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Invoice_Detail.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Invoice_Detail,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(INDET:Invoice_Sequence_Key,'INDET:Invoice_Sequence_Key',0)
    SELF.AddKey(INDET:Invoice_Detail_Sequence_Key,'INDET:Invoice_Detail_Sequence_Key',1)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Invoice_Detail.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Invoices)


Hide:Relate:Invoice_Detail.Kill PROCEDURE

  CODE
  _Hide:Access:Invoice_Detail.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:Invoice_Detail &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Invoice_Detail)                     ! destroy the file manager

