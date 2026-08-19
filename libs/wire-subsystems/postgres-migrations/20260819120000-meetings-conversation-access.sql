-- Meeting conversations are created with access {invite, code} instead of
-- {private, invite}, so that they can be joined by conversation code
-- (WPB-28155). Backfill existing meeting conversations with the new set.
-- Access ints (accessToInt32): private=1, invite=2, link=3, code=4.
-- GroupConvType ints (fromEnum): group=0, channel=1, meeting=2.
UPDATE conversation SET access = '{2,4}' WHERE group_conv_type = 2;
