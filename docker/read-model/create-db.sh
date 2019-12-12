#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    CREATE TABLE game_state (
        value text PRIMARY KEY
    );

    INSERT INTO game_state (value) VALUES
        ('in_progress'),
        ('draw'),
        ('yellow_won'),
        ('red_won');

    CREATE TABLE color (
        value text PRIMARY KEY
    );

    INSERT INTO color (value) VALUES
        ('red'),
        ('yellow');    

    CREATE TABLE player (
        value text PRIMARY KEY
    );    

    INSERT INTO player (value) VALUES
        ('red'),
        ('yellow'),
        ('none');

    CREATE TABLE games_internal(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        version int NOT NULL,
        game_state text NOT NULL REFERENCES game_state(value),
        moves integer[] NOT NULL,
        player_red uuid NOT NULL,
        player_yellow uuid NOT NULL
    );

    CREATE TABLE games(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        game_state text NOT NULL REFERENCES game_state(value),
        moves integer[] NOT NULL,
        player text NOT NULL REFERENCES player(value)
    );    

    CREATE FUNCTION game(
        my_id uuid,
        game_id uuid)
    RETURNS SETOF games AS
    '   SELECT id,
               serial_id,
               game_state,
               moves,
               CASE WHEN my_id = player_red THEN ''red''
                    WHEN my_id = player_yellow THEN ''yellow''
                    ELSE ''none''
               END
        FROM games_internal
        WHERE id = game_id
    ' LANGUAGE sql STABLE;    

    CREATE FUNCTION live_games(my_id uuid)
    RETURNS SETOF games AS
    '   SELECT id, serial_id, game_state, moves, ''none''
        FROM games_internal
        WHERE player_red <> my_id AND player_yellow <> my_id
    ' LANGUAGE sql STABLE;    

    CREATE FUNCTION my_games(my_id uuid)
    RETURNS SETOF games AS
    '   SELECT id,
               serial_id,
               game_state,
               moves,
               CASE WHEN my_id = player_red THEN ''red''
                    WHEN my_id = player_yellow THEN ''yellow''
                    ELSE ''none''
               END
        FROM games_internal
        WHERE player_red = my_id OR player_yellow = my_id
    ' LANGUAGE sql STABLE;     

    CREATE TABLE challenges_internal(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        client_id uuid NOT NULL,
        color text NOT NULL REFERENCES color(value)
    );

    CREATE TABLE challenges(
        id uuid NOT NULL PRIMARY KEY,
        serial_id serial NOT NULL,
        color text NOT NULL REFERENCES color(value)
    );

    CREATE FUNCTION villain_challenges(my_id uuid)
    RETURNS SETOF challenges AS
    '   SELECT id, serial_id, color
        FROM challenges_internal
        WHERE client_id <> my_id
    ' LANGUAGE sql STABLE;    

    CREATE FUNCTION my_challenges(my_id uuid)
    RETURNS SETOF challenges AS
    '   SELECT id, serial_id, color
        FROM challenges_internal
        WHERE client_id = my_id
    ' LANGUAGE sql STABLE;

    ---- TEST DATA (remove later) ----

    INSERT INTO challenges_internal (id, client_id, color) VALUES 
        ('d9fe283b-9d7d-4475-bcfe-a7ba1627778f', '521197f6-e33c-4ecb-99f7-07262d9080e1', 'red'),
        ('837df279-44fe-45b9-adc8-252440caeda4', '230ce3e8-18b6-4906-ac6c-b0ed7b3c13e6', 'yellow'),
        ('837df279-44fe-45b9-adc8-252440caeda5', '230ce3e8-18b6-4906-ac6c-b0ed7b3c13e7', 'yellow'),
        ('837df279-44fe-45b9-adc8-252440caeda6', '230ce3e8-18b6-4906-ac6c-b0ed7b3c13e8', 'red');

    INSERT INTO games_internal (id, version, game_state, player_red, player_yellow, moves) VALUES
        ('740bfbc6-7508-4d4f-baf9-858467330228', 5, 'in_progress',  '837df279-44fe-45b9-adc8-252440caeda6', '137df279-44fe-45b9-adc8-252440caeda6', '{5, 4, 5, 4}'),
        ('740bfbc6-7508-4d4f-baf9-858467330229', 1, 'in_progress',  '937df279-44fe-45b9-adc8-252440caeda6', '147df279-44fe-45b9-adc8-252440caeda6', '{}'),
        ('740bfbc6-7508-4d4f-baf9-858467330230', 10, 'in_progress', '107df279-44fe-45b9-adc8-252440caeda6', '157df279-44fe-45b9-adc8-252440caeda6', '{0, 1, 2, 3, 4, 5, 6, 0, 1}'),
        ('740bfbc6-7508-4d4f-baf9-858467330231', 4, 'in_progress',  '117df279-44fe-45b9-adc8-252440caeda6', '167df279-44fe-45b9-adc8-252440caeda6', '{5, 4, 5}'),
        ('740bfbc6-7508-4d4f-baf9-858467330232', 2, 'in_progress',  '127df279-44fe-45b9-adc8-252440caeda6', '177df279-44fe-45b9-adc8-252440caeda6', '{3}');
EOSQL
