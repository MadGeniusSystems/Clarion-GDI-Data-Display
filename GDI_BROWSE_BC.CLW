  MEMBER('GDI_Browse.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('GDI_Browse_BC0.CLW')
GDI_Browse_BC0:DctInit             PROCEDURE()
GDI_Browse_BC0:DctKill             PROCEDURE()
GDI_Browse_BC0:FilesInit           PROCEDURE()
    END
  END

DctInit PROCEDURE()
  CODE
  GDI_Browse_BC0:DctInit
  GDI_Browse_BC0:FilesInit


DctKill PROCEDURE()
  CODE
  GDI_Browse_BC0:DctKill

