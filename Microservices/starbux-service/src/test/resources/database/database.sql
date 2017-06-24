CREATE TABLE Status(
    statusId INTEGER PRIMARY KEY AUTOINCREMENT,
    status TEXT);
INSERT INTO Status (status) VALUES ('Ordered'), ('Prepared'), ('Payed'), ('Served');

CREATE TABLE Drink(
    drinkId INTEGER PRIMARY KEY AUTOINCREMENT,
    drink TEXT,
    cost INTEGER);
INSERT INTO Drink (drink, cost) VALUES ('coffee', 1), ('tea', 2);

CREATE TABLE Addition(
    additionId INTEGER PRIMARY KEY AUTOINCREMENT,
    addition TEXT,
    cost INTEGER);
INSERT INTO Addition (addition, cost) VALUES ('milk', 1), ('sugar', 1), ('honey', 2);

CREATE TABLE `Order`(
    orderId INTEGER PRIMARY KEY AUTOINCREMENT,
    statusId INTEGER,

    FOREIGN KEY (statusId) REFERENCES Status(statusId)
);

CREATE TABLE order_drink(
    id VARCHAR(64) PRIMARY KEY,
    orderId INTEGER,
    drinkId INTEGER,
    drinkCost INTEGER,

    FOREIGN KEY (orderId) REFERENCES `Order`(orderId),
    FOREIGN KEY (drinkId) REFERENCES Drink(drinkId)
);

CREATE TABLE order_drink_additions(
    orderDrinkId VARCHAR(64),
    additionId INTEGER,
    additionCost INTEGER,

    FOREIGN KEY (orderDrinkId) REFERENCES order_drink(id),
    FOREIGN KEY (additionId) REFERENCES Addition(additionId)
);

--insert into `Order` (statusId) values (1), (1);
--insert into order_drink (id, orderId, drinkId, drinkCost) values
--    ('3d0a656a-a8f7-4824-a749-0e720de6d53d', 1, 1, 1), -- order 1, coffee
--    ('c46e7a59-df10-4df0-a858-97d8fcf06b1a', 1, 2, 2), -- order 1, tea
--    ('2bd2b2fe-d972-4b3a-b9a1-665c849a352b', 1, 2, 2), -- order 1, tea
--
--    ('093dd39f-1608-4e1c-9cbc-f8c31e587675', 2, 1, 1); -- order 2, coffee
--insert into order_drink_additions (orderDrinkId, additionId, additionCost) values
--    ('3d0a656a-a8f7-4824-a749-0e720de6d53d', 1, 1), -- coffee, milk
--    ('3d0a656a-a8f7-4824-a749-0e720de6d53d', 2, 1), -- coffee, sugar
--    ('c46e7a59-df10-4df0-a858-97d8fcf06b1a', 3, 2); -- tea, honey

CREATE VIEW OrderView AS
    SELECT `Order`.orderId AS orderId,
           Status.status AS status,
           order_drink.id AS drinkId,
           Drink.drink AS drink,
           Drink.cost AS drinkCost,
           Addition.addition AS addition,
           Addition.cost AS additionCost
    FROM `Order`
        LEFT OUTER JOIN order_drink ON `Order`.orderId = order_drink.orderId
        LEFT OUTER JOIN Status ON `Order`.statusId = Status.statusId
        LEFT OUTER JOIN order_drink_additions ON order_drink.id = order_drink_additions.orderDrinkId
        LEFT OUTER JOIN Drink ON order_drink.drinkId = Drink.drinkId
        LEFT OUTER JOIN Addition ON order_drink_additions.additionId = Addition.additionId;
