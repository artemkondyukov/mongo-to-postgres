DROP TABLE Availabilities;
DROP TABLE RDKFingerprints;
DROP TABLE Compounds;
DROP TABLE Units;

CREATE TABLE IF NOT EXISTS Compounds (
  id SERIAL PRIMARY KEY,
  smiles VARCHAR (256) UNIQUE NOT NULL
); 

CREATE TABLE IF NOT EXISTS RDKFingerprints (
  id SERIAL PRIMARY KEY,
  compound INTEGER REFERENCES Compounds (id),
  fingerprint BIT(4096)
);

CREATE TABLE IF NOT EXISTS Units (
  id SERIAL PRIMARY KEY,
  unit VARCHAR(16)
); 
       
CREATE TABLE IF NOT EXISTS Availabilities (
  id SERIAL PRIMARY KEY,
  compound INTEGER REFERENCES Compounds (id),
  amount FLOAT,
  unit INTEGER REFERENCES Units (id)
);

INSERT INTO Units (unit) VALUES ('mg'); 
INSERT INTO Units (unit) VALUES ('g'); 
INSERT INTO Units (unit) VALUES ('kg'); 
INSERT INTO Units (unit) VALUES ('ml'); 
INSERT INTO Units (unit) VALUES ('l');