CREATE TABLE IF NOT EXISTS `documents` (
	`name`	TEXT,
	`url`	TEXT,
	`excerpt`	TEXT,
	`wordsCount`	INTEGER,
	`fileSize`	INTEGER
);

CREATE INDEX IF NOT EXISTS `documents_url` ON `documents` (`url`);
