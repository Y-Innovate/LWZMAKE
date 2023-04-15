* DSECT for an AVL node
*
AVLNODE                      DSECT
*
AVLNODE_NAME                 DS    A
AVLNODE_VALUE                DS    CL(VARIANT_SIZ)
AVLNODE_HEIGHT               DS    F
AVLNODE_PARENT               DS    A
AVLNODE_KID                  DS    2A
AVLNODE_PREV                 DS    A
AVLNODE_NEXT                 DS    A
*
AVLNODE_SIZ                  EQU   *-AVLNODE
*
* Properties for a AVL object
*
AVL_obj                      DSECT
AV2_obj                      EQU   *
                             DS    CL(COM_obj_SIZ)   * Generic COM
                             DS    0F
*
AVL_obj_attrs                EQU   *
AV2_obj_attrs                EQU   *
*
AVLNODE_root                 DS    A
nodeCount                    DS    F
firstNode                    DS    A
lastNode                     DS    A
*
AVL_obj_attrs_SIZ            EQU   *-AVL_obj_attrs
AV2_obj_attrs_SIZ            EQU   *-AV2_obj_attrs
*
                             DS    0F
*
AVL_obj_SIZ                  EQU   *-AVL_obj
AV2_obj_SIZ                  EQU   *-AV2_obj
