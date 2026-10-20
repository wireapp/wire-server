module LintOpenAPI.Report
  ( formatViolations,
    formatViolation,
    summarize,
    renderRoute,
    renderRouteKey,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Imports
import LintOpenAPI.Types

formatViolations :: Int -> [ViolationContext] -> Text
formatViolations ignoredCount [] =
  if ignoredCount > 0
    then "No breaking changes detected (" <> Text.pack (show ignoredCount) <> " ignored)."
    else "No breaking changes detected."
formatViolations ignoredCount ctxs =
  let grouped = Map.fromListWith (++) [(ctx.baselineVersion, [ctx]) | ctx <- reverse ctxs]
      sortedGroups = Map.toDescList grouped
      formatGroup (ver, groupCtxs) =
        let verLabel = case ver of
              Just v -> "v" <> Text.pack (show v)
              Nothing -> "baseline"
         in "### Breaking changes against "
              <> verLabel
              <> "\n\n"
              <> Text.unlines (map formatViolation groupCtxs)
   in Text.intercalate "\n" (map formatGroup sortedGroups)
        <> "\n"
        <> summarize ignoredCount ctxs

-- | Format a single violation with its context.
formatViolation :: ViolationContext -> Text
formatViolation ctx =
  let routeLabel = renderRouteKey ctx.routeKey
      opIdLabel = case ctx.routeId of
        Just oid -> " (" <> oid <> ")"
        Nothing -> ""
      detail = renderViolation ctx.violation
   in "- " <> routeLabel <> opIdLabel <> ": " <> detail

-- | Render a route key as "METHOD /path/{param}".
renderRouteKey :: RouteKey -> Text
renderRouteKey rk =
  Text.pack (show rk.method) <> " " <> renderRoute rk.route

-- | Render a normalized route back to path format.
renderRoute :: NormalizedRoute -> Text
renderRoute nr =
  "/" <> Text.intercalate "/" (map renderSegment nr.segments)

-- | Render a single route segment.
renderSegment :: RouteSegment -> Text
renderSegment = \case
  Literal t -> t
  Placeholder -> "{_}"

-- | Render a violation detail message.
renderViolation :: Violation -> Text
renderViolation = \case
  RouteRemoved ->
    "Route removed"
  QueryParamRemoved name ->
    "Query parameter removed: \"" <> name <> "\""
  RequiredQueryParamAdded name ->
    "Required query parameter added: \"" <> name <> "\""
  RequiredBodyFieldAdded name ->
    "Required body field added: \"" <> name <> "\""
  ResponseFieldRemoved name ->
    "Response field removed: \"" <> name <> "\" (was required)"
  EnumValueRemoved field val ->
    let fieldPart =
          if Text.null field
            then ""
            else " from field \"" <> field <> "\""
     in "Enum value removed" <> fieldPart <> ": \"" <> val <> "\""
  EnumValueAdded field val ->
    let fieldPart =
          if Text.null field
            then ""
            else " to field \"" <> field <> "\""
     in "Enum value added" <> fieldPart <> ": \"" <> val <> "\""

-- | Generate a summary line.
summarize :: Int -> [ViolationContext] -> Text
summarize ignoredCount ctxs =
  let count = length ctxs
      versions =
        Set.fromList
          [v | ViolationContext {baselineVersion = Just v} <- ctxs]
      verCount = Set.size versions
      ignoredText =
        if ignoredCount > 0
          then " (" <> Text.pack (show ignoredCount) <> " ignored)"
          else ""
   in "Summary: "
        <> Text.pack (show count)
        <> " breaking change"
        <> (if count == 1 then "" else "s")
        <> ignoredText
        <> " detected across "
        <> Text.pack (show verCount)
        <> " baseline version"
        <> (if verCount == 1 then "" else "s")
