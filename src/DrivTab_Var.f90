MODULE DrivTab_Var

    INTEGER, PARAMETER :: maxnode=100              ! REWRITE if changed
    INTEGER, PARAMETER :: maxstate=6               ! REWRITE if changed
    INTEGER, PARAMETER :: maxsubtab=7              ! REWRITE if changed
    INTEGER, PARAMETER :: maxtune=8                ! REWRITE if changed
    INTEGER, PARAMETER :: mflx=5                   ! REWRITE if changed
    REAL,    PARAMETER :: omit=-9999.              ! REWRITE if changed
!
    REAL,    SAVE              :: bnds   ( 2, maxstate, maxsubtab)
    INTEGER, SAVE, ALLOCATABLE :: bt     ( :)
    REAL,    SAVE              :: dsig   ( maxstate, maxsubtab)
    INTEGER, SAVE              :: ioffs  ( maxsubtab)
    INTEGER, SAVE              :: iprt
    INTEGER, SAVE              :: isloaded = -1
    INTEGER, SAVE              :: itdim  ( maxsubtab)
    INTEGER, SAVE              :: nsizes ( maxstate, maxsubtab)
    INTEGER, SAVE              :: nsv    ( maxsubtab)
    INTEGER, SAVE              :: ntbl
    INTEGER, SAVE              :: ntv    ( maxsubtab)
    INTEGER, SAVE              :: os     ( maxstate, maxsubtab)
    REAL,    SAVE              :: rng    ( maxstate, maxsubtab)
!
!
END MODULE DrivTab_Var
