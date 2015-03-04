-- datetimes in RedShift are stored as the INT8 type as Unix time
-- all RedShift times are in

create table employees (
  version INT2 encode mostly8 NOT NULL,
  id VARCHAR(256) encode raw NOT NULL,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  family_name VARCHAR(256) encode raw NOT NULL,
  given_name VARCHAR(256) encode raw NOT NULL,
  preferred_name VARCHAR(256) encode raw NOT NULL,
  PRIMARY KEY (id, site_id, pos)
)
  distkey(site_id)
  sortkey(id, site_id, pos);

create table orders (
  version INT2 encode mostly8 NOT NULL,
  id VARCHAR(256) encode raw NOT NULL,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  currency CHAR(3) encode bytedict NOT NULL,
  order_discount_count INT2 encode raw NOT NULL,
  employee_id VARCHAR(256) encode raw NOT NULL,
  day_part VARCHAR(256) encode bytedict NULL,
  total_item_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_order_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  item_discount_count DECIMAL(18,5) encode mostly16 NOT NULL,
  gross_sales DECIMAL(18,5) encode mostly16 NOT NULL,
  net_sales DECIMAL(18,5) encode mostly16 NOT NULL,
  net_total DECIMAL(18,5) encode mostly16 NOT NULL,
  total_collected DECIMAL(18,5) encode mostly16 NOT NULL,
  sub_total DECIMAL(18,5) encode mostly16 NOT NULL,
  order_sale_amount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_tip DECIMAL(18,5) encode mostly16 NOT NULL,
  total_tax DECIMAL(18,5) encode mostly16 NOT NULL,
  total_cost DECIMAL(18,5) encode mostly16 NOT NULL,
  PRIMARY KEY (id, site_id, pos),
  FOREIGN KEY (employee_id, site_id, pos) REFERENCES employees(id, site_id, pos)
)
  distkey(site_id)
  sortkey(created);

create table items (
  version INT2 encode mostly8 NOT NULL,
  id VARCHAR(256) encode raw NOT NULL,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  order_id VARCHAR(256) encode raw NOT NULL,
  name VARCHAR(256) encode raw NULL,
  item_catalog_id VARCHAR(256) encode raw NULL,
  quantity DECIMAL(18,5) encode mostly8 NOT NULL,
  currency CHAR(3) encode bytedict NOT NULL,
  price DECIMAL(18,5) encode mostly16 NOT NULL,
  item_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  order_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  item_discount_count INT2 encode raw NOT NULL,
  tax_class VARCHAR(256) encode bytedict NOT NULL,
  revenue_category VARCHAR(256) encode bytedict NULL,
  sku VARCHAR(256) encode raw NULL,
  menu VARCHAR(256) encode bytedict NULL,
  status VARCHAR(256) encode bytedict NULL,
  cost DECIMAL(18,5) encode mostly16 NULL,
  total_item_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_order_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  tax DECIMAL(18,5) encode mostly16 NOT NULL,
  total_tax DECIMAL(18,5) encode mostly16 NOT NULL,
  gross_sales DECIMAL(18,5) encode mostly16 NOT NULL,
  total_cost DECIMAL(18,5) encode mostly16 NOT NULL,
  PRIMARY KEY (id, site_id, pos),
  FOREIGN KEY (order_id, site_id, pos) REFERENCES orders(id, site_id, pos)
)
  distkey(site_id)
  sortkey(order_id, site_id, pos);

create table modifiers (
  version INT2 encode mostly8 NOT NULL,
  id VARCHAR(256) encode raw NOT NULL,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  item_id VARCHAR(256) encode raw NOT NULL,
  order_id VARCHAR(256) encode raw NOT NULL,
  name VARCHAR(256) encode raw NULL,
  modifier_catalog_id VARCHAR(256) encode raw NULL,
  quantity DECIMAL(18,5) encode mostly8 NOT NULL,
  currency CHAR(3) encode bytedict NOT NULL,
  price DECIMAL(18,5) encode mostly16 NOT NULL,
  item_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  order_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  tax_class VARCHAR(256) encode bytedict NOT NULL,
  revenue_category VARCHAR(256) encode bytedict NULL,
  sku VARCHAR(256) encode raw NULL,
  modifier_group VARCHAR(256) encode bytedict NULL,
  cost DECIMAL(18,5) encode mostly16 NULL,
  total_item_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_order_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  total_discount DECIMAL(18,5) encode mostly16 NOT NULL,
  tax DECIMAL(18,5) encode mostly16 NOT NULL,
  total_tax DECIMAL(18,5) encode mostly16 NOT NULL,
  gross_sales DECIMAL(18,5) encode mostly16 NOT NULL,
  total_cost DECIMAL(18,5) encode mostly16 NOT NULL,
  PRIMARY KEY (id, site_id, pos),
  FOREIGN KEY (item_id, site_id, pos) REFERENCES items(id, site_id, pos),
  FOREIGN KEY (order_id, site_id, pos) REFERENCES orders(id, site_id, pos)
)
  distkey(site_id)
  sortkey(id, site_id, pos);

create table payments (
  version INT2 encode mostly8 NOT NULL,
  id VARCHAR(256) encode raw NOT NULL,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  order_id VARCHAR(256) encode raw NOT NULL,
  amount DECIMAL(18,5) encode mostly16 NOT NULL,
  tax DECIMAL(18,5) encode mostly16 NOT NULL,
  tip DECIMAL(18,5) encode mostly16 NOT NULL,
  discount_allowance DECIMAL(18,5) encode mostly16 NOT NULL,
  currency CHAR(3) encode bytedict NOT NULL,
  method VARCHAR(256) encode bytedict NOT NULL,
  type VARCHAR(256) encode bytedict NOT NULL,
  employee_id VARCHAR(256) encode raw NOT NULL,
  PRIMARY KEY (id, site_id, pos),
  FOREIGN KEY (employee_id, site_id, pos) REFERENCES employees(id, site_id, pos)
)
  distkey(site_id)
  sortkey(order_id, site_id, pos);

create table customers (
  version INT2 encode mostly8 NOT NULL,
  key VARCHAR(36) encode raw NOT NULL UNIQUE,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  id VARCHAR(256) encode raw NULL,
  order_id VARCHAR(256) encode raw NOT NULL,
  item_id VARCHAR(256) encode raw NOT NULL,
  payment_id VARCHAR(256) encode raw NOT NULL,
  name VARCHAR(256) encode raw NOT NULL,
  card_type VARCHAR(256) encode bytedict NULL,
  card_first_four CHAR(4) encode bytedict NULL,
  card_last_four CHAR(4) encode bytedict NULL,
  payment_token VARCHAR(256) encode raw NULL,
  loyalty_id VARCHAR(256) encode raw NULL,
  email VARCHAR(256) encode raw NULL,
  phone VARCHAR(256) encode raw NULL,
  PRIMARY KEY (key, site_id, pos),
  FOREIGN KEY (order_id, site_id, pos) REFERENCES orders(id, site_id, pos),
  FOREIGN KEY (item_id, site_id, pos) REFERENCES items(id, site_id, pos),
  FOREIGN KEY (payment_id, site_id, pos) REFERENCES payments(id, site_id, pos)
)
  distkey(site_id)
  sortkey(order_id, site_id, pos);

create table tables
(
  version INT2 encode mostly8 NOT NULL,
  key VARCHAR(36) encode raw NOT NULL UNIQUE,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  id VARCHAR(256) encode raw NULL,
  order_id VARCHAR(256) encode raw NOT NULL,
  name VARCHAR(256) encode raw NULL,
  seats INT2 encode raw NOT NULL,
  PRIMARY KEY (key, site_id, pos),
  FOREIGN KEY (order_id, site_id, pos) REFERENCES orders(id, site_id, pos)
)
  distkey(site_id)
  sortkey(order_id, site_id, pos);

create table taxes (
  version INT2 encode mostly8 NOT NULL,
  id VARCHAR(256) encode raw NOT NULL,
  site_id VARCHAR(256) encode bytedict NOT NULL,
  pos VARCHAR(256) encode bytedict NOT NULL,
  created INT8 encode delta NOT NULL,
  updated INT8 encode delta NOT NULL,
  imported INT8 encode delta NOT NULL,
  formalized INT8 encode delta NOT NULL,
  order_id VARCHAR(256) encode raw NOT NULL,
  item_id VARCHAR(256) encode raw NOT NULL,
  modifier_id VARCHAR(256) encode raw NOT NULL,
  amount DECIMAL(18,5) encode mostly16 NOT NULL,
  currency CHAR(3) encode bytedict NOT NULL,
  authority VARCHAR(256) encode bytedict NOT NULL,
  rate DECIMAL(18,5) encode mostly8 NOT NULL,
  PRIMARY KEY (id, site_id, pos),
  FOREIGN KEY (order_id, site_id, pos) REFERENCES orders(id, site_id, pos),
  FOREIGN KEY (item_id, site_id, pos) REFERENCES items(id, site_id, pos),
  FOREIGN KEY (modifier_id, site_id, pos) REFERENCES modifiers(id, site_id, pos)
)
  distkey(site_id)
  sortkey(id, site_id, pos);
