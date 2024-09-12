@EndUserText.label: 'Projection view: ZFIN_USER_C'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@ObjectModel.semanticKey: ['AuthUser']
@UI.headerInfo:{
    title: { type:  #STANDARD, value: 'AuthUser' },
    description.value: 'Name',
    typeName:       'User of FIN Planning',
    typeNamePlural: 'Users of FIN Planning',
    typeImageUrl:   'sap-icon://account'
}
define root view entity ZFIN_USER_C
  provider contract transactional_query
  as projection on ZFIN_USER_I
{
      @UI.facet: [{ id:              'maintain',
                    purpose:         #STANDARD,
                    type:            #IDENTIFICATION_REFERENCE,
                    label:           'User Maintenance',
                    position:        10
                  },
                  { id:              'ChangeLog',
                    type:            #FIELDGROUP_REFERENCE,
                    label:           'Change Log',
                    position:        20,
                    targetQualifier: 'ChangeLog'
                  }]

      @UI: { lineItem:       [{ position: 10 }],
             identification: [{ position: 10 }],
             selectionField: [{ position: 10 }] }
  key AuthUser,

      @UI: { lineItem:       [{ position: 20 }],
             identification: [{ position: 20 }],
             selectionField: [{ position: 20 }] }
  key ValidFr,

      @UI: { lineItem:       [{ position: 30 }],
             identification: [{ position: 30 }],
             selectionField: [{ position: 30 }] }
  key ValidTo,

      @UI: { lineItem:       [{ position: 40 }],
             identification: [{ position: 40 }],
             selectionField: [{ position: 40 }] }
      Name,

      /*-- Admin Data --*/
      @UI.fieldGroup: [{ qualifier: 'ChangeLog', position: 10 }]
      CreatedBy,

      @UI.fieldGroup: [{ qualifier: 'ChangeLog', position: 20 }]
      CreatedAt,

      @UI.fieldGroup: [{ qualifier: 'ChangeLog', position: 30 }]
      LastChangedBy,

      @UI.fieldGroup: [{ qualifier: 'ChangeLog', position: 40 }]
      LastChangedAt,

      @UI.hidden: true
      ExcelRowNumber
}
