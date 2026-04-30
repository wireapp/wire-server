# Multi-Ingress Cross-IdP SSO Migration

## Problem Statement

In multi-ingress Wire deployments, each ingress domain (e.g., `ernie.example.com`, `bert.example.com`) can have its own SAML IdP. Currently, when a user logs in via SSO on different ingresses with different IdPs, separate user accounts are created even if the SAML NameID (email) is identical.

This causes:
1. **Duplicate accounts**: The same person has multiple Wire accounts
2. **Email conflicts**: If using email-based NameID, the second login fails with "email already in use"

## Solution

Implement automatic user recognition and IdP migration for email-based SAML NameIDs in multi-ingress setups.

## Design

### Location

The change is in `verdictHandlerResultCore` in `services/spar/src/Spar/App.hs:441`, in the `Nothing` branch where a user is not found by UserRef or old issuers.

### Algorithm

```
When user not found by UserRef or old issuers:
1. Check if NameID is email format (pattern match UNameIDEmail)
2. If yes:
   a. Extract email from NameID
   b. Look up user by email using BrigAccess.getByEmail
   c. If user found AND user's team matches IdP's team:
      - Create new UserRef with current IdP's issuer
      - Call moveUserToNewIssuer to migrate SSO binding
      - Return the existing user's ID
   d. If user not found OR team mismatch:
      - Fall through to autoprovision new user
3. If NameID is not email:
   - Autoprovision new user (current behavior)
```

### New Function

```haskell
getUserByEmailAndMigrateToNewIdP ::
  ( Member BrigAccess r,
    Member SAMLUserStore r,
    Member (Logger String) r
  ) =>
  IdP ->
  SAML.UserRef ->
  SAMLEmail.Email ->
  Sem r (Maybe UserId)
getUserByEmailAndMigrateToNewIdP idp newUref email = do
  let emailAddr = Intra.emailFromSAML (CI.original email)
      team' = idp ^. idpExtraInfo . team
  mUser <- BrigAccess.getByEmail emailAddr
  case mUser of
    Just usr | userTeam usr == Just team' -> do
      -- Found user in same team - migrate to new IdP
      case userSSOId usr of
        Just (UserSSOId oldUref) -> do
          Logger.log Logger.Info $ "Migrating user " <> show (userId usr) <> " to new IdP"
          moveUserToNewIssuer oldUref newUref (userId usr)
          pure $ Just (userId usr)
        _ -> pure Nothing -- User has no SSO ID, can't migrate
    _ -> pure Nothing -- User not found or wrong team
```

### Modified Code Path

```haskell
-- In verdictHandlerResultCore, replace:
Nothing -> do
  traceM $ "XXX - uref " <> show uref
  buid <- Id <$> Random.uuid
  autoprovisionSamlUser idp buid uref
  validateSamlEmailIfExists buid uref
  pure buid

-- With:
Nothing -> do
  case uref of
    SAML.UserRef _ (view SAML.nameID -> UNameIDEmail email) -> do
      -- Try to find existing user by email and migrate
      mExistingUid <- getUserByEmailAndMigrateToNewIdP idp uref email
      case mExistingUid of
        Just uid -> pure uid
        Nothing -> autoprovisionNewUser idp uref
    _ -> autoprovisionNewUser idp uref
  where
    autoprovisionNewUser idp' uref' = do
      buid <- Id <$> Random.uuid
      autoprovisionSamlUser idp' buid uref'
      validateSamlEmailIfExists buid uref'
      pure buid
```

## Security Considerations

1. **Team verification**: The found user MUST belong to the same team as the IdP. This prevents cross-team account hijacking where an attacker could claim any email address by configuring an IdP in their own team.

2. **Email verification**: `BrigAccess.getByEmail` only returns users with verified emails, ensuring the migration is based on a trusted identifier.

3. **SSO ID requirement**: Migration only occurs if the existing user has a SAML SSO ID. Users without SSO bindings are not affected.

4. **Logging**: Migration events are logged for audit purposes.

## Test Plan

### Modified Test: `testCrossIdpSsoEmailConflict`
- **Current behavior**: Second login fails with "email already in use"
- **New behavior**: Second login succeeds and returns the same user ID

### New Test: `testCrossIdpSsoEmailMigration`
- User logs in on ingress A with email NameID → Account created
- Same user logs in on ingress B with same email → Same account returned
- Verify user's SSO ID now references IdP2's issuer
- Verify user can re-login on both ingresses

### Existing Test: `testCrossIdpSsoCreatesDistinctUsers`
- This test uses username-based NameID (not email)
- Should continue to work unchanged (duplicate users created)

### New Test: `testCrossIdpSsoRejectsCrossTeamMigration`
- User in Team A logs in via Team A's IdP with email
- Attacker in Team B configures IdP with same email
- Login via Team B should create new user (not hijack Team A user)

## Files Changed

1. `services/spar/src/Spar/App.hs`
   - Add `getUserByEmailAndMigrateToNewIdP` function
   - Modify `verdictHandlerResultCore` to call it

2. `integration/test/Test/Spar/MultiIngressCrossIdpSso.hs`
   - Modify `testCrossIdpSsoEmailConflict` to expect success
   - Add `testCrossIdpSsoEmailMigration` test
   - Add `testCrossIdpSsoRejectsCrossTeamMigration` test

## Dependencies

Uses existing functions:
- `BrigAccess.getByEmail` - look up user by email
- `moveUserToNewIssuer` - update SSO binding
- `Intra.emailFromSAML` - convert SAML email to EmailAddress
