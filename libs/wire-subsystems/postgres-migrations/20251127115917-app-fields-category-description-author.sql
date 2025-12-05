ALTER TABLE apps
      ADD COLUMN category text DEFAULT 'other' NOT NULL,
      ADD COLUMN description text DEFAULT '' NOT NULL,
      ADD COLUMN author text;
