@EndUserText.label: 'Root View Entity: ZFIN_USER_I'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZFIN_USER_I
  as select from ztfin_user

  /*-- Associations --*/
  // association [0..1] to /DMO/I_Agency   as _Agency   on $projection.agency_id     = _Agency.AgencyID
  // association [0..1] to /DMO/I_Customer as _Customer on $projection.customer_id   = _Customer.CustomerID
  // association [0..1] to I_Currency      as _Currency on $projection.currency_code = _Currency.Currency

{
      @EndUserText.label: 'User Name'
  key authuser        as AuthUser,

      @EndUserText.label: 'User Valid From'
      @Semantics.systemDateTime.createdAt: true
  key valid_fr        as ValidFr,

      @EndUserText.label: 'User Valid To'
      @Semantics.systemDateTime.createdAt: true
  key valid_to        as ValidTo,

      @EndUserText.label: 'Name and Surname'
      name            as Name,

      /*-- Admin Data --*/
      @EndUserText.label: 'Created By'
      @Semantics.user.createdBy: true
      created_by      as CreatedBy,

      @EndUserText.label: 'Created At'
      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,

      @Semantics.user.lastChangedBy: true
      @EndUserText.label: 'Last Changed By'
      last_changed_by as LastChangedBy,

      @Semantics.systemDateTime.lastChangedAt: true
      @EndUserText.label: 'Last Changed At'
      last_changed_at as LastChangedAt,

      0               as ExcelRowNumber

      /*-- Public Associations --*/
      // _Agency,
      // _Customer,
      // _Currency
}
