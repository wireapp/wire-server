-- Create index to support case-insensitive channel search ordering and pagination
-- Matches queries filtering by team and group_conv_type and ordering by lower(name), id
CREATE INDEX conversation_team_group_type_lower_name_id_idx
  ON conversation (team, group_conv_type, lower(name), id);
