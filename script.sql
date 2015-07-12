DROP DATABASE IF EXISTS `django_db1`;
CREATE DATABASE `django_db1` DEFAULT CHARACTER SET utf8 DEFAULT COLLATE utf8_general_ci;

USE 'django_db1';
GRANT ALL PRIVILEGES ON django_db1.* TO 'aris'@'localhost' IDENTIFIED BY 'aris'

WITH GRANT OPTION;
FLUSH PRIVILEGES;
