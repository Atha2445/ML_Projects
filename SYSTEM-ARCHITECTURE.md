# Shop Billing — complete system architecture

Reference for debugging. Written on 22 Oct 2026 from the code as it is today
(nothing was changed while writing it). It replaces the older diagrams in
`docs/architecture.html` (14 Oct), which do not show later work.

---

## 0. How to read this document

Every feature has a tag that says **where it came from**:

| Tag | Meaning |
|---|---|
| `[ORIGINAL]` | Was already in your project before Claude started (your first code or its README "still to build" list) |
| `[ASKED]` | You asked for it |
| `[APPROVED]` | Claude proposed it and you said yes |
| `[CLAUDE]` | Claude added it **without being asked** |

To find something in the code, search for the **function name** given (line
numbers change; names don't). All paths are inside `shop-billing-system/`.

Contents:
1. The big picture
2. Folder and file map
3. What happens when the app starts
4. Where data is stored
5. Database tables (every column)
6. Shop details (config) — where they come from
7. Logins and roles — who can do what
8. The bridge: every call from screen to back office
9. Back-office services (every function)
10. Screens (every tab, field and button)
11. Key flows, step by step
12. GST, discount and profit maths
13. Printing
14. Build and deploy
15. Tests
16. Feature origin index
17. Known problems and loose ends (found while writing this)

---

## 1. The big picture

The app is an **Electron** desktop program. Electron = a window (drawn with
HTML/CSS/JS) + a Node.js back office. They are separate processes and can
only talk through a small, fixed list of messages.

```
┌──────────────────────────── Shop's Windows PC ─────────────────────────────┐
│                                                                            │
│  WINDOW (renderer)                     BACK OFFICE (main process)          │
│  src/renderer/                         src/main/                           │
│  ┌──────────────────────┐   preload    ┌───────────────────────────────┐   │
│  │ index.html           │   bridge     │ main.js                       │   │
│  │ styles.css           │ ───────────▶ │  · startup, window, menu      │   │
│  │ js/app.js  (tabs)    │ window.      │  · role check on every call   │   │
│  │ js/ui.js   (helpers) │ shopAPI.*    │  · backup timer, sync timer   │   │
│  │ js/views-*.js        │ ◀─────────── │ services (one per area):      │   │
│  │  (screens)           │  results     │  auth, shop, config, billing, │   │
│  └──────────────────────┘              │  payment, account, product,   │   │
│        ▲ can NOT touch disk,           │  customer, farm, backup,      │   │
│          database or internet          │  sync, logger, secrets        │   │
│                                        └──────────────┬────────────────┘   │
│                                                       │ better-sqlite3     │
│                                        ┌──────────────▼────────────────┐   │
│                                        │ shop_data.db  (SQLite file)   │   │
│                                        │ backups/  logs/  config/      │   │
│                                        └───────────────────────────────┘   │
└──────────────────────────────────────────────────────┬─────────────────────┘
                                                       │ HTTPS every 60 s
                                                       ▼ (NOT CONNECTED yet)
                                        Zoho Catalyst cloud  [ORIGINAL]
                                        catalyst-backend/
```

Why the split: the window shows text typed by staff, so it is kept away from
the database. Cost price and profit never leave the back office for a staff
login. `[ORIGINAL]` idea, enforced by `[CLAUDE]` code in `main.js → handle()`.

Main libraries (`electron-app/package.json`):

| Library | Version | Job |
|---|---|---|
| electron | 42.11.9 | Desktop app shell `[APPROVED]` (you chose Electron 42) |
| better-sqlite3 | 12.11.1 | Talks to the SQLite database file `[ORIGINAL]` (upgraded) |
| electron-builder | 26.16.1 | Makes the Windows installer `[ORIGINAL]` (upgraded) |
| puppeteer-core | 22.15.0 | Drives the app in the end-to-end test only `[CLAUDE]` |

---

## 2. Folder and file map

```
shop-billing-system/
├── CLAUDE.md, README.md, DEPLOYMENT.md       guidelines, overview, install steps
├── key.txt                                  YOUR file: holds Gajanan's vendor reset key (keep private)
├── Logo.png, Entry.mp4                      your originals (copied into the app)
├── catalyst-backend/                        [ORIGINAL] cloud side, not connected
│   ├── data-store-schema/schema.md          cloud tables: Shops, Customers, Bills, BillItems, BillAdminMetadata
│   └── functions/billing-sync-function/     receives synced records
├── docs/
│   ├── SYSTEM-ARCHITECTURE.md               this file
│   ├── architecture.html, images/           older diagrams (14 Oct)
│   └── ui-preview/                          farm-look design work, photos, screenshots
└── electron-app/                            THE APP
    ├── package.json                         versions, npm scripts, installer settings
    ├── config/shop.json + README.md         shop details built into the installer
    ├── clients/gajanan-krushi-seva-kendra/  [CLAUDE] saved copy of Gajanan's shop.json
    ├── build/icon.png                       app icon
    ├── scripts/                             build helpers (section 14)
    ├── src/db/database.js                   creates/upgrades all tables
    ├── src/main/                            back office (section 9)
    ├── src/renderer/                        window (section 10)
    │   ├── index.html                       header, tabs, icon set, script order
    │   ├── styles.css                       farm look + print rules
    │   ├── assets/                          entry.mp4, logo-full.jpg, logo-mark.png, photos/
    │   └── js/                              app.js, ui.js, views-*.js
    ├── test/                                unit tests + e2e (section 15)
    ├── dev-data/                            your test database when running from VS Code (not shipped)
    └── dist/                                built installer (.exe)
```

Back-office files (`electron-app/src/main/`):

| File | Lines | Job |
|---|---|---|
| main.js | 440 | Startup, window, menu, backup timer, sync timer, **all permission checks** |
| preload.js | 90 | The bridge: list of calls the window may make |
| authService.js | 280 | Logins, roles, passwords, lockout, recovery code |
| shopService.js | 106 | Shop profile, owner's licence/GST settings, first-run setup |
| configService.js | 122 | Reads shop.json (which copy, signature image, vendor key hash) |
| billingService.js | 572 | Bill maths, save, cancel, find, summary |
| billLedger.js | 94 | Shared money helpers: rounding, numbers, status, payments |
| paymentService.js | 182 | Receive payment, void payment, list |
| billingAccountService.js | 130 | Customer credit (udhaar) accounts and limits |
| productService.js | 218 | Products, suggestions, stock, hide/show |
| customerService.js | 139 | Customers, phone/Aadhaar checks, purchase history |
| farmService.js | 153 | Farm details, crops, soil reports, farm insights |
| backupService.js | 178 | Daily backups, second copy, restore checks |
| syncService.js | 118 | Push queue to Zoho Catalyst `[ORIGINAL]` |
| logger.js | 42 | Log file |
| secrets.js | 37 | Hashing and code generation |

Window files (`electron-app/src/renderer/js/`), loaded in this order by `index.html`:

| File | Lines | Job |
|---|---|---|
| ui.js | 276 | Helpers: `h()` builds elements, `screenlet()` card, `field()`, `select()`, `pill()`, `icon()`, `dataTable()`, `flash()`, `withSuggestions()`, `searchPicker()`, `focusSoon()`, money/date formatting |
| views-auth.js | 222 | Opening video, setup, login, recovery code, forgot password |
| views-billing.js | 1258 | Bills, New Bill, bill view, printed invoice, Payments, Accounts, Summary |
| views-products.js | 268 | Products list and product form |
| views-customers.js | 388 | Customers list, customer page, farm editor |
| views-settings.js | 369 | Settings cards |
| app.js | 194 | Tabs, who is logged in, menu shortcuts, sync counter, start-up |

---

## 3. What happens when the app starts

Double-click the desktop icon →

| # | Where | What |
|---|---|---|
| 1 | main.js (top) | Pick the **data folder** (section 4) `[CLAUDE]` |
| 2 | main.js | Start the log file `<data>/logs/app.log` `[CLAUDE]` |
| 3 | main.js | Tell configService about `<data>/config/` (on-site shop details) `[CLAUDE]` |
| 4 | main.js | **Single instance**: if the app is already open, bring that window forward and quit `[CLAUDE]` |
| 5 | main.js `whenReady` | `db.initDatabase()`: create missing tables, add missing columns (safe on old data) |
| 6 | main.js | Read shop.json. If broken → error box "Shop details missing" and quit |
| 7 | main.js `buildMenu()` | Menu bar File / Edit / View / Help `[CLAUDE]` |
| 8 | main.js `createWindow()` | Window 1280×800 (min 1024×640), opens **maximised**; blocks navigation away and pop-ups |
| 9 | main.js `scheduleBackups()` | First backup check after 15 s, then every 3 h `[CLAUDE]` |
| 10 | main.js | Sync attempt every 60 s (fails quietly: not configured) `[ORIGINAL]` |
| 11 | app.js `boot()` | Plays the opening video **while** asking "is anyone set up / logged in?" `[ASKED]` |
| 12 | app.js | No users yet → **Setup** screen. Someone still logged in → app. Else → **Login** |
| 13 | app.js `enterApp()` | Fill header (shop name, tagline, user, role), build tabs for that role, open first tab (staff → New Bill, others → Bills `[CLAUDE]`) |

The login is held **in memory** in the back office (`authService → currentUser`).
Closing the app logs everyone out.

---

## 4. Where data is stored

**Data folder** (chosen in main.js at startup):

| Situation | Folder |
|---|---|
| Installed app on Windows | `C:\Users\<name>\AppData\Roaming\shop-billing-system\` |
| Running from VS Code (`npm start`) | `electron-app/dev-data/` `[APPROVED]` |
| Env var `SHOP_BILLING_DATA_DIR` set | that folder (tests use this) |

Inside the data folder:

| Path | What | Origin |
|---|---|---|
| `shop_data.db` (+ `-wal`, `-shm`) | **All data**: bills, customers, products, users, payments, farm reports, settings | `[ORIGINAL]` |
| `backups/shop_data-YYYYMMDD-HHMMSS.db` | Daily automatic backups, newest 14 kept | `[CLAUDE]` |
| `backups/before-restore-…db` | Copy made just before a restore | `[CLAUDE]` |
| `logs/app.log` (+ `app.log.1`) | Log, rotated at 1 MB | `[CLAUDE]` |
| `config/shop.json` | On-site shop details (only if made via Help menu) | `[CLAUDE]` |
| Second-copy folder (USB/Drive) `…/shop-billing-backups/` | Copy of each daily backup, newest 30 kept | `[CLAUDE]` |
| OS temp `shop-billing-reports/` | A soil report written out when "Open" is clicked | `[CLAUDE]` |

The program files themselves go to `C:\Users\<name>\AppData\Local\Programs\shop-billing-system\`.
Uninstalling does **not** delete the data folder.

---

## 5. Database tables (every column)

Defined in `electron-app/src/db/database.js`. First block = `CREATE TABLE`,
then `migrateColumns()` adds later columns to old databases, then
`widenBillPaymentMethods()` rebuilds `bills` once so cheque and net banking
are allowed.

### 5.1 Tables from your original project `[ORIGINAL]`

**shop_profile** (one row; a copy of the shop details for the cloud)
`id`(always 1), `shop_name`, `gst_number`, `license_number`, `address`,
`sector_profile`(unused), `catalyst_shop_id`(unused), `created_at`.
Written by `shopService.syncProfileToDb()` at every start and after saving Licence & GST.

**users**
`id`, `username`(unique, lower-case), `password_hash`(scrypt), `role`(owner/staff/auditor), `created_at`
+ added `[CLAUDE]`: `display_name`, `active`(1 = can log in), `last_login_at`, `recovery_hash`, `recovery_set_at`.

**products**
`id`, `name`, `company`, `size_weight`, `cost_price`(before GST), `sell_price`(includes GST), `stock_qty`,
`expiry_date`, `hsn_code`, `gst_rate`, `reason_for_stocking`, `created_at`, `updated_at`
+ added: `reorder_level`(low-stock line) `[CLAUDE]`, `active`(0 = hidden from billing) `[CLAUDE]`,
`purchase_tax_type`('intra' = CGST+SGST, 'inter' = IGST) `[ASKED]`.

**customers**
`id`, `name`, `phone`(10 digits, stored without +91), `area`(village), `address`, `created_at`
+ added `aadhaar`(12 digits, never printed) `[ASKED]`.

**bills**
`id`, `bill_number`(SHOP-YYYYMMDD-0001), `customer_id`, `billing_date`(UTC), `subtotal`(price × qty before discount),
`extra_charges`(always 0 now), `discount`(sum of row discounts), `cgst_amount`, `sgst_amount`, `total_amount`,
`payment_method`(card/cash/cheque/bank_transfer/upi/credit), `transaction_ref`, `additional_terms`(unused),
`created_by_user_id`, `sync_status`(pending/synced/failed), `created_at`
+ added: `status`(unpaid/partially_paid/paid/cancelled) `[ASKED]`, `amount_paid` `[ASKED]`, `due_date` `[CLAUDE]`,
`cancelled_reason`, `cancelled_at` `[ASKED]`, `current_crop`(copy of farmer's crop, never printed) `[ASKED]`,
`gst_type`(shop's GST type when the bill was made) `[ASKED]`.

**bill_items** (rows of a bill; a snapshot so later product edits don't change old bills)
`id`, `bill_id`, `product_id`, `item_name`, `company`, `size_weight`, `expiry_date`(not filled now), `quantity`,
`unit_price`, `line_total`(what the customer pays for the row), `unit_cost_price`(for profit)
+ added: `hsn_code`, `gst_rate`, `taxable_amount`, `tax_amount` `[ASKED]` (GST),
`discount_type`('amount' = ₹/unit, 'percent'), `discount_value`, `discount_amount` `[ASKED]`.

**bill_admin_metadata** (owner/auditor only, never printed)
`bill_id`, `total_profit`, `profit_margin_pct`, `notes`.

**sync_queue** (changes waiting for the cloud)
`id`, `entity_type`(bill/customer/product), `entity_id`, `operation`(create/update), `payload_json`, `attempts`, `last_error`, `created_at`.

**audit_log** (who changed what)
`id`, `user_id`, `action`, `entity_type`, `entity_id`, `before_json`, `after_json`, `created_at`.
Actions written: `user.login`, `user.create`, `user.update`, `user.reset_password`, `user.change_password`,
`user.recover_code`, `user.recover_vendor_key`, `user.new_recovery_code`, `product.create`, `product.update`,
`product.stock_adjust`, `product.activate`, `product.deactivate`, `bill.cancel`, `payment.void`,
`farm.report_delete`, `shop.tax_details`.

### 5.2 Tables added for "billing like OFBiz" `[ASKED]`

**payments**: `id`, `payment_number`(PAY-YYYYMMDD-0001), `customer_id`, `amount`, `method`, `transaction_ref`, `notes`,
`status`(received/voided), `void_reason`, `received_at`, `created_by_user_id`.

**payment_applications**: `id`, `payment_id`, `bill_id`, `amount_applied` — which bill each payment paid.

**billing_accounts**: `id`, `customer_id`(unique), `credit_limit`(0 = no limit) `[CLAUDE]`, `description`, `created_at`.

**bill_status_history**: `id`, `bill_id`, `status`, `note`, `user_id`, `changed_at`.

### 5.3 Tables added for farm details `[ASKED]`

**customer_farm**: `customer_id`, `total_acres`, `current_crop`, `updated_at`.
**customer_crops**: `id`, `customer_id`, `season`(kharif/rabi), `crop`, `year`.
**customer_documents**: `id`, `customer_id`, `kind`('soil_test'), `file_name`, `mime_type`, `size_bytes`,
`data`(the file itself, inside the database `[CLAUDE]` choice), `uploaded_by_user_id`, `uploaded_at`.

### 5.4 Settings table `[CLAUDE]`

**app_settings** (key → value). Keys in use:

| Key | Written by | Meaning |
|---|---|---|
| `last_auto_backup_at` | backupService | Time of last daily backup |
| `extra_backup_folder` | backupService | Second-copy folder ('' = none) |
| `last_extra_backup_at` | backupService | Time of last second copy |
| `last_extra_backup_error` | backupService | Why the second copy failed |
| `shop.license_number` | shopService | Owner's licence no. (wins over shop.json) `[ASKED]` |
| `shop.gst_number` | shopService | Owner's GSTIN (wins over shop.json) `[ASKED]` |
| `shop.gst_type` | shopService | regular / composition / none `[APPROVED]` |

Indexes: bills by date, customer, status; products by expiry; customers by phone;
payments by customer; payment_applications by bill and payment; status history by bill;
crops and documents by customer.

---

## 6. Shop details (config) — where they come from

`configService.loadShopConfig()` reads **the first that exists**:

```
1. folder in env var SHOP_BILLING_CONFIG_DIR      (tests)
2. <data folder>/config/shop.json                 (made at the shop via Help → Shop Details File…) [CLAUDE]
3. electron-app/config/shop.json                  (built into the installer) [APPROVED: one file per client]
```

Then `shopService.getProfile()` lays the owner's Settings values on top:
`license_number`, `gst_number`, `gst_type` from app_settings win over the file.
If the owner never saved GST type: `gst_enabled: true` in the file → Regular, else → Not registered.

shop.json keys: `shop_name`, `tagline`, `address`, `phone`, `email`, `gst_number`, `license_number`,
`seller_name`, `signature_image`(file next to shop.json), `gst_enabled`, `invoice_footer`,
`vendor_reset_key_hash`(hash only).

Read once and **cached** until restart. Editing shop.json needs Help → Restart.

"Details missing" check (`configService.isPlaceholder`): shop_name, address, phone or seller_name empty
or still sample text → yellow banner in the app and a warning on login `[CLAUDE]`.

---

## 7. Logins and roles — who can do what

Roles existed in your original table `[ORIGINAL]`. The enforcement is in `main.js → handle(channel, roles, fn)`:
every call checks the logged-in user's role **in the back office**, so hiding a button is never the only guard.

Groups used in main.js: `ANY` = owner, staff, auditor · `CASHIERS` = owner, staff ·
`MANAGERS` = owner, auditor · `OWNER` = owner only · `null` = no login needed.

| Can… | Owner | Staff | Auditor |
|---|---|---|---|
| Make bills, take payments | ✔ | ✔ | ✘ |
| Change bill date (earlier only) | ✔ | ✘ (ignored) | ✘ |
| See cost, profit, margin | ✔ | ✘ | ✔ |
| See customers list, phone, Aadhaar, purchase history | ✔ | ✘ | ✔ |
| Cancel bill, void payment | ✔ | ✘ | ✘ |
| Add/edit products, stock, hide | ✔ | ✘ | ✘ |
| Edit customers, farm details, delete reports | ✔ | ✘ | ✘ |
| Credit limits | ✔ | ✘ | ✘ |
| Logins, backups, recovery code, Licence & GST | ✔ | ✘ | ✘ |

Tabs shown (`app.js → NAV`): Bills (all), New Bill (owner, staff), Payments (all),
Accounts (owner, auditor), Products (all), Customers (owner, auditor),
Summary (owner, auditor), Settings (all; contents differ).

Login rules (`authService.js`) `[CLAUDE]`: username 3–32 chars (letters, numbers, `.` `-` `_`),
password 6+ chars, 5 wrong tries → 30 s lock (kept in memory, a restart clears it),
there must always be one active owner, an owner can't remove their own owner access.

---

## 8. The bridge: every call from screen to back office

Window code calls `api.<group>.<name>()` (`window.shopAPI`, defined in `preload.js`),
which sends channel `<group>:<name>` to `main.js`, which checks the role and calls the service.

| Channel | Roles | Service function | Origin |
|---|---|---|---|
| auth:status | none | shopService.needsSetup + current user + profile | [ORIGINAL plan] |
| auth:setup | none | shopService.completeSetup | [ORIGINAL plan] |
| auth:login / auth:logout | none | authService.login / logout | [ORIGINAL plan] |
| auth:changePassword | ANY | authService.changeOwnPassword | [CLAUDE] |
| auth:recover | none | authService.recoverOwner | [CLAUDE] |
| auth:recoveryStatus / auth:newRecoveryCode | OWNER | authService.getRecoveryStatus / regenerateRecoveryCode | [CLAUDE] |
| users:list / create / update / resetPassword | OWNER | authService.* | [ORIGINAL plan] |
| shop:get | ANY | shopService.getProfile | [ORIGINAL] |
| shop:saveTaxDetails | OWNER | shopService.saveTaxDetails | [ASKED] |
| billing:create | CASHIERS | billingService.createBill (staff: date removed) | [ORIGINAL] |
| billing:preview | CASHIERS | billingService.previewBill | [ASKED] |
| billing:getById / list | ANY | getBillById / listBills | [ORIGINAL] |
| billing:getAdminView | MANAGERS | getBillAdminView | [ORIGINAL] |
| billing:cancel | OWNER | cancelBill | [ASKED] |
| billing:summary | MANAGERS | getSummary | [CLAUDE] |
| billing:shopProfile | ANY | shopService.getProfile | [ORIGINAL] |
| billing:savePdf | ANY | main.js: print window to PDF | [ORIGINAL plan] |
| payment:receive | CASHIERS | paymentService.receivePayment | [ASKED] |
| payment:void | OWNER | paymentService.voidPayment | [ASKED] |
| payment:list | ANY | paymentService.listPayments | [ASKED] |
| account:due | ANY | billingAccountService.getDue | [ASKED] |
| account:list / get | MANAGERS | listAccounts / getAccount | [ASKED] |
| account:upsert | OWNER | upsertAccount (credit limit) | [CLAUDE] |
| product:search | ANY | searchProducts — **not used by any screen** | [CLAUDE] |
| product:suggest | ANY | suggestItems | [ASKED] |
| product:list | ANY | listProducts (cost only for managers) | [APPROVED] |
| product:get | ANY | getProduct — **not used by any screen** | [CLAUDE] |
| product:alerts | ANY | getStockAlerts | [CLAUDE] |
| product:save | OWNER | saveProduct | [APPROVED] |
| product:adjustStock | OWNER | adjustStock | [CLAUDE] |
| product:setActive | OWNER | setActive (hide/show) | [CLAUDE] |
| customer:search | ANY | searchCustomers (id, name, area only) | [ORIGINAL] |
| customer:list | MANAGERS | listCustomers | [CLAUDE] |
| customer:upsert | OWNER | upsertCustomer | [ORIGINAL] |
| customer:getPurchaseHistory | MANAGERS | getPurchaseHistory | [ORIGINAL] |
| farm:get | ANY | farmService.getFarm | [ASKED] |
| farm:save / deleteReport | OWNER | saveFarm / deleteReport | [ASKED] |
| farm:insights | MANAGERS | getFarmInsights | [CLAUDE] |
| farm:openReport | MANAGERS | write file to temp, open in PC's viewer | [ASKED] |
| backup:status / now / chooseFolder / clearFolder / openFolder / restore | OWNER | backupService + dialogs in main.js | [CLAUDE] |
| sync:now / status | ANY | syncService | [ORIGINAL] |
| `menu` (back office → window) | — | menu click → app.js `api.onMenu` | [CLAUDE] |

---

## 9. Back-office services (every function)

### authService.js
`hasUsers`, `insertUser`(validates, lower-cases username), `publicUser`, `login`, `startSession`, `logout`,
`getCurrentUser`, `requireUser(roles)`(the permission gate), `listUsers`, `createUser`, `updateUser`,
`resetPassword`, `changeOwnPassword`, `issueRecoveryCode`, `recoverOwner`, `regenerateRecoveryCode`, `getRecoveryStatus`.

### secrets.js `[CLAUDE]`
`hashSecret` → `scrypt$<salt>$<hash>`; `verifySecret`; `makeCode(groups)` → e.g. `7KQM-X3RT-PV9C-H2WD`
(letters without 0/O/1/I/L); `normaliseCode` (ignores dashes, spaces, case).
Recovery code = 4 groups; vendor reset key = 5 groups.

### shopService.js
`getProfile`(file + owner settings), `saveTaxDetails`(checks GSTIN format `^\d{2}[A-Z]{5}\d{4}[A-Z][1-9A-Z]Z[0-9A-Z]$`,
GSTIN required unless "Not registered", licence ≤ 80 chars, writes audit), `syncProfileToDb`,
`completeSetup`(only when no users exist; creates owner; returns one-time recovery code), `needsSetup`.

### configService.js
`setSiteDir`, `loadShopConfig`(cached), `ensureSiteConfig`(copies current shop.json + signature to
`<data>/config/`, without the vendor key hash), `isGstEnabled`(file value only — no longer used by billing),
`getVendorResetHash`(on-site file first, else installer's), `isPlaceholder`.
Signature: PNG/JPG up to 2 MB, sent to the window as a `data:` image.

### billingService.js
- `lineDiscount` — one row's discount (section 12).
- `computeTotals(db, payload)` — all bill maths; refuses items not in the product list (section 12).
- `previewBill` — totals while typing (no cost/profit).
- `resolveBillingDate` — owner's earlier date → timestamp; refuses future dates, years before 2000, impossible dates.
- `queueBillSync` — puts a bill (with profit) in sync_queue.
- `createBill` — the save (section 11.1).
- `cancelBill` — section 11.4.
- `getBillById` — public bill (no phone, no Aadhaar, no profit).
- `getBillAdminView` — adds phone, profit, per-row profit (owner/auditor).
- `listBills` — Find Bills filters, newest first, 100 max by default (the Bills screen asks for 200).
- `getSummary` — sales, GST, profit, collections by method, total due `[CLAUDE]`.

### billLedger.js
`EPSILON` = 0.005 (half a paisa counts as zero), `round2`, `nextDocumentNumber`(PREFIX-YYYYMMDD-NNNN, counts
today's rows), `deriveStatus`, `recordStatusChange`, `insertPayment`, `applyPayment`, `refreshBillPaymentStatus`.

### paymentService.js `[ASKED]`
`receivePayment`(one bill, or spread over the customer's open bills oldest first), `voidPayment`(reason required),
`getPayment`, `listPayments`. Methods: cash, upi, card, bank_transfer, cheque.

### billingAccountService.js `[ASKED]`
`assertCreditAvailable`(opens an account on the first unpaid balance; enforces limit `[CLAUDE]`),
`getOutstanding`, `getDue`, `listAccounts`, `getAccount`, `upsertAccount`.

### productService.js
`searchProducts`(unused), `suggestItems`(active products only, name or company contains text,
names starting with it first, 20 max) `[ASKED]`, `listProducts`(filters: active/low/expiring/inactive),
`getProduct`, `getStockAlerts` `[CLAUDE]`, `cleanProduct`(GST 0–40, purchase type intra/inter, name required),
`saveProduct`(opening stock only when new; audit), `adjustStock`(± with reason) `[CLAUDE]`, `setActive` `[CLAUDE]`.
Expiry warning = 30 days `[CLAUDE]`.

### customerService.js
`cleanPhone`(drops spaces, dashes, +91 or leading 0; must be 10 digits) `[ASKED]`,
`cleanAadhaar`(12 digits) `[ASKED]`, `upsertCustomer`, `searchCustomers`(20 max, no phone),
`listCustomers` `[CLAUDE]`, `getPurchaseHistory`(bills + top 10 items) `[ORIGINAL]`.

### farmService.js `[ASKED]`
`hasFarmData`, `cleanFarm`(acres 0–100000, season kharif/rabi, year 1950–next year, crop ≤ 80 chars),
`cleanReports`(PDF/PNG/JPG recognised by file contents, ≤ 10 MB), `saveFarmInTxn`(crop list replaced, reports added),
`saveFarm`, `getFarm`, `getReport`, `deleteReport`, `getFarmInsights` `[CLAUDE]`.

### backupService.js `[CLAUDE]`
`backupTo`(SQLite online backup, then verifies the copy), `runAutoBackup`(skips if one was made in the last
20 h; keeps 14; copies to the second folder, keeps 30 there), `inspectBackup`(must be SQLite, have
bills/bill_items/customers/payments/users, pass a quick integrity check), `listAutoBackups`, `getStatus`,
`setExtraFolder`, `replaceDatabaseFile`, `stampedName`, `autoBackupDir`.

### syncService.js `[ORIGINAL]`
`setAuthToken`(never called — no cloud login yet), `callCatalystFunction`, `syncPendingChanges`
(50 at a time, 5 tries each), `getSyncStatus`. Needs env var `CATALYST_PROJECT_DOMAIN` + a token: neither exists,
so every run stops with "Catalyst not configured" and the queue only grows.

### logger.js `[CLAUDE]`
`init`, `info`, `warn`, `error`, `folder`. Also catches crashes (`uncaughtException`, `unhandledRejection`).

---

## 10. Screens (every tab, field and button)

### 10.1 Always on screen (after login) — `index.html`, `app.js`
- **Header photo** (onion field) with shop name + tagline `[APPROVED]` (farm look). No Gajatantra logo here `[ASKED]`.
- **Pill top-right**: "N waiting to sync" / "All synced" `[CLAUDE]`, user name, role, **Log out**.
- **Tabs** with line icons and a sliding green line `[APPROVED]`; Accounts (ledger) and Settings (gear) icons `[CLAUDE]`.
- **Yellow banner** when shop details are placeholders `[CLAUDE]`.
- **Flash message** bottom-right (4 s, errors 8 s).

### 10.2 Opening video, setup, login — `views-auth.js`
- **Video** `assets/entry.mp4` full screen; click/any key skips; gives up after 15 s `[ASKED]`.
- Background: slow-zooming wheat photo, framed Gajatantra logo `[APPROVED]`.
- **Setup** (first start): Your name, Username, Password (6+), Confirm → "Create owner login" → **recovery code page** `[CLAUDE]`.
- **Recovery code page**: code shown once, Print, tick "I have written down…", Continue `[CLAUDE]`.
- **Login**: Username, Password, Log in; link "Use the recovery code" `[CLAUDE]`.
- **Reset owner password**: Owner username, Recovery code or provider key, New password, Confirm `[CLAUDE]`.

### 10.3 Bills tab — `renderFindBills`
- Season banner (photo + season + date) `[APPROVED]`. Months: Jun–Sep Kharif, Oct–Jan Rabi sowing, Feb–Mar Rabi harvest, Apr–May Summer.
- Owner only: "Backups are only on this computer…" notice until a second-copy folder is set `[CLAUDE]`.
- **Find Bills**: Search (bill no. or customer), Status, Payment method, From, To, Find, Clear, "+ New Bill".
- **Bills table**: Bill no., Date, Customer, Method, Total, Paid, Due, Status (with icon) + total row. Click → bill.

### 10.4 New Bill tab — `renderNewBill`
- Season banner `[APPROVED]`; card icons on this screen only `[APPROVED]`.
- **New bill card**: Bill date (owner only; earlier dates only) `[APPROVED]`, Mode of payment (Card, Cash, Cheque,
  Net banking / account transfer, UPI) `[ASKED]`, Reference no. (hidden for cash), Customer name (suggestions)
  `[ASKED]`, Location `[ASKED]`, Phone (+91 fixed, 10 digits) and Aadhaar no. (12 digits) — both optional `[ASKED]`.
  Blue chip "Returning customer…" when picked from suggestions.
- **Items card**: Product name (type → suggestions from product list only) `[ASKED]`, Company and Weight (filled
  from product), Quantity, Price (from product, cannot be typed — `[CLAUDE]` reading of your answer), Discount
  box + "₹ / unit" or "%" `[ASKED]`, Total, × remove, "+ Add row". Red line when a typed name isn't in the list `[ASKED]`.
  Enter moves to the next box; Enter in a last-row discount adds a row.
- **Farm details (optional)** fold-out: Total area (acres), Current crop, crops (Kharif/Rabi, crop, year),
  soil test files `[ASKED]`.
- **Bill total card**: Total of all items, Discount, (Regular GST: Taxable value, CGST, SGST), Bill total,
  Received now, Balance on credit; Amount received now (₹); Balance due by `[CLAUDE]`; Save bill → sprout animation
  (1.2 s) → bill opens `[APPROVED]`.

### 10.5 Bill page — `renderBill`, `renderInvoice`
- Buttons: Receive payment, Cancel bill (owner; only if nothing paid), Print, Save PDF, ← Bills.
- The printed invoice (section 13).
- Owner/auditor only: **Profit on this bill** (profit, margin, customer phone, note, per-item cost/profit) `[ORIGINAL]` data.
- **Status history** table `[ASKED]`.

### 10.6 Payments tab — `renderPayments` `[ASKED]`
- "Receive payment on account": pick customer → Amount, Method, Reference, Note → Record payment.
- Filters From/To; table: Payment, Date, Customer, Method, Ref, Applied to, Amount, Status, Void (owner).

### 10.7 Accounts tab — `renderAccounts`, `renderAccount` `[ASKED]`
- Table: Customer, Phone, Area, Credit limit, Due, Available, Open bills.
- Owner: "Open or edit an account": Credit limit (0 = no limit), Description `[CLAUDE]`.
- One account: details, open bills (with Due date), recent payments, Receive payment.

### 10.8 Products tab — `views-products.js` `[APPROVED]`
- Search, Show (All / Low stock / Expiring within 30 days / Hidden from billing) `[CLAUDE]`, warning buttons `[CLAUDE]`.
- Table: Product, HSN `[CLAUDE]`, GST, Price, (managers) Cost, Actual cost `[ASKED]`, Margin `[CLAUDE]`,
  Stock, Reorder at `[CLAUDE]`, Expiry.
- Form (owner): Product name, Company, Weight (50/45/40/25/10 kg list) `[ASKED]`, HSN code `[CLAUDE]`,
  GST rate (0/5/12/18/28/40) `[ASKED]`, Selling price incl. GST, Cost price before GST, Bought from
  (CGST+SGST / IGST) `[ASKED]`, live line "Cost + CGST + SGST (or IGST) = Actual cost" `[ASKED]`,
  Opening stock, Warn when stock is at `[CLAUDE]`, Expiry date, Why we stock it, Margin hint `[CLAUDE]`.
- Edit only: Change stock (± with reason) `[CLAUDE]`, Hide from billing / Show in billing `[CLAUDE]`.

### 10.9 Customers tab — `views-customers.js` `[CLAUDE]`
- Search, "+ Add customer", table: Customer, Phone, Location, Bills, Total bought, Due, Last bill.
- Customer page: Phone (+91), Aadhaar `[CLAUDE]`, Location, Address, Bills, Total bought, Due now; Billing account button;
  bills list; most bought items `[ORIGINAL]` data; Farm details card (owner can edit) `[ASKED]`;
  Contact details form (owner) with Aadhaar `[CLAUDE]`.

### 10.10 Summary tab — `renderSummary` `[CLAUDE]`
From, To, Show, Today. Cards: Bills, Sales, Profit, GST charged, Money received, Still due on these bills,
Cancelled bills; "Money received by method" table; All customers → Total due today;
Farm data → Farmers with details, Land covered, With a soil test, Current crops table.

### 10.11 Settings tab — `views-settings.js`
| Card | Who | Contents | Origin |
|---|---|---|---|
| My account | all | name, username, role, change password | [CLAUDE] |
| Backup & restore | owner | Backup now, Restore from file, second-copy folder, last backups, Restore | [CLAUDE] |
| Owner recovery code | owner | make a new code with current password | [CLAUDE] |
| Logins | owner | list, + Add login, edit role/access/password | [ORIGINAL plan] |
| Licence & GST | owner edits, others view | Licence no., GSTIN, GST type | [ASKED] / type [APPROVED] |
| Shop details | all | read-only shop.json values + signature | [APPROVED] |
| Cloud backup | all | waiting / stuck counts, Sync now | [CLAUDE] (sync [ORIGINAL]) |

### 10.12 Menu bar — `main.js → buildMenu()` `[CLAUDE]`
- File: New Bill F2, Find Bills F3, Payments F4, Products F5, Print Bill Ctrl+P, Save Bill as PDF Ctrl+S, Log Out, Quit.
- Edit (standard), View (zoom, full screen; reload/dev tools only when not installed).
- Help: Shop Details File…, Restart Shop Billing, Open Data Folder, Open Log Folder, About.

---

## 11. Key flows, step by step

### 11.1 Save a bill — `billingService.createBill`
```
Window (renderNewBill)                           Back office
──────────────────────                           ───────────
checks: customer name, phone 10 digits,
Aadhaar 12, every typed product picked,
at least one product
api.billing.create(payload) ───────────────▶  main.js: role CASHIERS; staff → date removed
                                              1. customer name required; phone/Aadhaar checked
                                              2. payment method must be in the list
                                              3. resolveBillingDate (owner's earlier date)
                                              4. computeTotals (section 12)
                                              5. amount received: blank = full total
                                                 (credit = 0); must be 0…total
                                              ── ONE TRANSACTION (all or nothing) ──
                                              6. new customer → insert; returning → update
                                                 phone/Aadhaar if typed
                                              7. farm details → customer's profile
                                              8. balance left → open account / check limit
                                              9. bills row (+ status history "Bill created")
                                             10. bill_items rows; stock_qty −= quantity
                                             11. bill_admin_metadata (profit)
                                             12. money received → payments + application
                                             13. sync_queue row
◀───────────────────────────────────────────  returns public bill
flash "Bill … saved", sprout 1.2 s, open bill
```

### 11.2 Receive payment — `paymentService.receivePayment`
Amount > 0, method valid. With a bill: that bill only (not cancelled). Without: the customer's open bills,
oldest first. Refuses more than is due. Each bill's paid amount and status are recalculated and logged.

### 11.3 Void payment — `voidPayment` (owner)
Reason required → payment marked voided → every bill it paid goes back to unpaid / partly paid → audit row.

### 11.4 Cancel bill — `cancelBill` (owner)
Reason required; refused if any money is on the bill (void payments first); stock is put back; status
cancelled; audit row; sync queued.

### 11.5 Daily backup — `runAutoBackup`
Timer (15 s after start, then every 3 h) → skip if last backup < 20 h ago → copy database → verify →
delete beyond 14 → copy to second folder (keep 30) → record time or error.

### 11.6 Restore — main.js `backup:restore` (owner)
Pick an automatic backup or a file → `inspectBackup` → confirm box (bills, customers, last bill) →
current data saved as `before-restore-…` → database closed → file replaced → app restarts.

### 11.7 Forgot owner password — `authService.recoverOwner`
Owner username + recovery code (or vendor reset key) + new password. Wrong tries count towards the
login lock. Recovery code used → a new one is shown once. Vendor key used → owner's code unchanged.

### 11.8 Owner saves Licence & GST — `shopService.saveTaxDetails`
Checks → 3 settings saved + audit row → shop_profile updated → window's `session.shop` replaced →
**new** bills use the new GST type; old bills keep theirs (`bills.gst_type`).

---

## 12. GST, discount and profit maths — `billingService.computeTotals`

For **each row** (product must exist and not be hidden):

```
price      = product.sell_price                      (includes GST)
gross      = round2(quantity × price)
discount   = ₹/unit:  quantity × value   (value may not exceed price)
             %:       gross × value / 100 (value may not exceed 100)
row amount = gross − discount                        (what the farmer pays)

GST type Regular:
  taxable  = round2(row amount ÷ (1 + rate/100))
  tax      = row amount − taxable
  CGST     = round2(tax ÷ 2),   SGST = tax − CGST
Composition / Not registered:
  taxable  = row amount, tax = 0, rate stored as 0

cost per unit = Regular: product.cost_price
                others:  round2(cost_price × (1 + rate/100))   [CLAUDE]
row profit    = taxable − quantity × cost per unit
```

Bill: `subtotal` = Σ gross, `discount` = Σ discounts, `cgst_amount`/`sgst_amount` = Σ row values,
`total_amount` = Σ row amounts, profit = Σ row profits, margin % = profit ÷ Σ taxable × 100.

Worked example (Regular, 5% GST):

| Row | Qty × price | Discount | Amount | Taxable | CGST | SGST |
|---|---|---|---|---|---|---|
| DAP | 2 × 1,350 = 2,700 | ₹50/unit = 100 | 2,600.00 | 2,476.19 | 61.91 | 61.90 |
| Urea | 4 × 266.50 = 1,066 | — | 1,066.00 | 1,015.24 | 25.38 | 25.38 |
| **Bill** | 3,766.00 | 100.00 | **3,666.00** | 3,491.43 | 87.29 | 87.28 |

Products screen uses the same rule for **Margin** (`views-products.js → margin()`); **Actual cost** there
is always cost × (1 + GST %), whatever the GST type.

Rounding: `round2` (to the paisa); anything under ₹0.005 counts as zero.

---

## 13. Printing — `renderInvoice` + `@media print` in styles.css

- Title: **TAX INVOICE** (Regular), **BILL OF SUPPLY** (Composition), **INVOICE** (Not registered).
  Older bills without `gst_type`: TAX INVOICE only if GST was charged.
- Top left: shop name, tagline, address, Ph., email, GSTIN (if set), **Licence (if set)** `[CLAUDE]` — see 17.1.
- Top right: Bill no., Date, Payment (+ ref), Due by (if any), Status.
- Bill to: name, village, address. **Never**: phone, Aadhaar, farm details, cost, profit.
- Rows (no GST): #, Product, Company, Weight, Qty, Price, [Discount], Amount.
- Rows (Regular): #, Product (company · weight under it), HSN, Qty, Price, [Discount], Taxable, GST %, CGST, SGST, Amount.
  Discount shows "₹50.00/unit" or "10%" and "− ₹100.00".
- Composition: line "Composition taxable person, not eligible to collect tax on supplies" `[CLAUDE]`.
- Totals, payments list, signature image + seller name + "Authorised signatory, <shop>", footer line.
- Print styles: black text on white, no colours, no header/tabs/buttons/photos `[ASKED]`.
- Save PDF: `billing:savePdf` prints the window to an A4 PDF file the user picks.

---

## 14. Build and deploy

npm scripts (`electron-app/package.json`):

| Script | Does | Origin |
|---|---|---|
| `npm start` | Run from source (data in dev-data/) | [ORIGINAL] |
| `npm test` | Unit tests | [CLAUDE] |
| `npm run test:e2e` | Drive the real app | [CLAUDE] |
| `npm run check-config` (also runs before every build) | Refuse placeholder shop details | [CLAUDE] |
| `npm run build:win:docker` | Windows installer on Linux (Docker, pinned image) | [APPROVED] |
| `npm run build:win` | Windows installer on a Windows PC | [ORIGINAL] |
| `npm run build:win:generic` | One installer for all shops | [CLAUDE] |
| `npm run make-vendor-key` | New reset key; hash into config/shop.json | [APPROVED] |
| `npm run make-logo` | Icons from the logo | [CLAUDE] |
| postinstall | Rebuild SQLite driver for Electron | — |

Installer (electron-builder, NSIS) `[ASKED]`: appId `com.shrigajatantra.shopbilling`, name "Shop Billing",
per-user (no admin), can change folder, **desktop + Start-menu shortcut**, keeps data on uninstall,
output `dist/Shop-Billing-Setup-<version>.exe`. Packaged files: `src/**`, `config/**`, `build/icon.png`, `package.json`.

`scripts/build-win-docker.sh`: checks config → copies project (without dist, dev-data, screenshots) to a temp
folder → builds inside `electron-builder:wine` (pinned digest) → copies the .exe back → prints sha256.
Building the Windows installer directly on Linux is refused (it breaks the local SQLite driver).

Per shop (DEPLOYMENT.md "Way A"): edit `config/shop.json` → `make-vendor-key` → `check-config` → build.

---

## 15. Tests `[CLAUDE]` (not shipped)

`npm test` runs each file with Electron's Node, on a temporary data folder:
- `access.test.js` (18) — setup, roles, lockout, staff/auditor limits, products, farm, suggestions.
- `billing.test.js` (23) — upgrades of old data, GST split, discounts, only-listed products, phone/Aadhaar,
  payments, credit limit, cancel/void, filters, summary, GST types, sync queue.
- `config.test.js` (5) — which shop.json is used.
- `deploy.test.js` (10) — recovery code, vendor key, lockout, backups, restore.

`npm run test:e2e` (9 steps, opens the real window, screenshots in `test/e2e/screenshots/`), using the sample
shop in `test/e2e/fixtures/config/`.

---

## 16. Feature origin index

### `[ORIGINAL]` (your project before Claude)
SQLite tables for shop, users with 3 roles, products (incl. HSN, GST rate, expiry, stock, reason for stocking),
customers (phone, area), bills, bill items, admin-only profit, sync queue, audit log; CGST/SGST idea; profit kept
apart; purchase history; Zoho Catalyst sync and cloud function. README "still to build": login, role guards, PDF.

### `[ASKED]`
OFBiz-style billing (payments, payment applications, billing accounts, status history, cancel, void);
opening video + logo; fixed shop details; owner changes bill date; 5 payment modes A–Z; customer and product
suggestions; bill row (product, company, weight 50/45/40/25/10/manual, qty, price, total); seller name and
signature; farm details (acres, Kharif/Rabi crops with year, current crop, soil test files) never printed;
desktop icon installer; farm look with real photos; less Gajatantra; simple print; owner Licence/GSTIN in
Settings; GST in products (slabs, CGST/SGST/IGST, actual cost); GST on bills; phone and Aadhaar on bills;
only listed products with typing suggestions; spelling fixes; per-row discount on the bill.

### `[APPROVED]`
Config file per client; one signatory; owner-only date; Electron 42; vendor reset key; Docker image in
workspace; dev-data/; farm look version 3 (season banner, sprout, tab line, card icons on New Bill);
one installer per shop; GST type chosen by owner; CGST+SGST only; profit on cost before GST; GST inside price;
per-item discount; phone/Aadhaar optional and not printed; Products screen (offered as "missing screens").

### `[CLAUDE]` (not asked)
| Area | Item | Where in code |
|---|---|---|
| Logins | 5-try / 30 s lock | authService `MAX_FAILED_LOGINS`, `LOCKOUT_MS` |
| Logins | Owner recovery code + reset screen | authService `issueRecoveryCode`, `recoverOwner`; views-auth |
| Logins | My account / change password | views-settings `myAccountScreenlet` |
| Safety | Daily backups, second copy, restore, Bills reminder | backupService; main.js `backup:*`; views-settings `backupScreenlet`; views-billing `renderFindBills` |
| Safety | Log file, crash catching | logger.js; main.js top |
| Safety | Single instance | main.js `requestSingleInstanceLock` |
| Config | On-site shop.json (Help menu), placeholder banner | configService `ensureSiteConfig`, `isPlaceholder`; index.html `#detailsBanner` |
| Build | check-config, generic installer, tests | scripts/check-config.js; package.json; test/ |
| Billing | Summary screen | billingService `getSummary`; views-billing `renderSummary` |
| Billing | Credit limit, Balance due by | billingAccountService `assertCreditAvailable`; New Bill `dueField` |
| Billing | Price locked to product | billingService `computeTotals`; New Bill row |
| Billing | ₹ discount per unit (not per row) | billingService `lineDiscount` |
| Billing | Composition/unregistered profit on cost + GST | billingService `computeTotals` `unitCost` |
| Billing | "Bill of supply" line, GSTIN format check | views-billing `renderInvoice`; shopService `GSTIN_FORMAT` |
| Billing | Removed bill-level discount / extra charges | billingService `computeTotals` (extra_charges always 0) |
| Billing | Licence printed on bills | views-billing `renderInvoice` |
| Customers | Customers tab, customer edit, Aadhaar on Customers screen | views-customers; customerService `listCustomers` |
| Customers | Returning customer's phone/Aadhaar updated from a bill | billingService `createBill` step 6 |
| Customers | Same name + village = same customer | views-billing `unlink` |
| Farm | Soil reports stored inside the database | farmService `saveFarmInTxn` |
| Farm | Farm insights | farmService `getFarmInsights`; Summary |
| Products | HSN on screens and invoice | views-products; renderInvoice |
| Products | Low-stock / expiry warnings, reorder level | productService `getStockAlerts`; views-products |
| Products | Stock change with reason | productService `adjustStock` |
| Products | Hide from billing | productService `setActive` |
| Products | Margin column and hint | views-products `margin()` |
| Screens | Menu bar + F2–F5, Ctrl+P/S | main.js `buildMenu`; app.js `api.onMenu` |
| Screens | Sync counter in header, Cloud backup card | app.js `refreshSyncStatus`; views-settings `syncScreenlet` |
| Screens | Staff open on New Bill | app.js `enterApp` |
| Screens | Ledger/gear tab icons, hover shadow | app.js `NAV`; styles.css `.screenlet:hover` |
| Unused | product:search, product:get | preload.js, main.js, productService |

---

## 17. Known problems and loose ends (found while writing this — **not fixed**)

1. **Licence prints on bills** although on 14 Oct you said "Don't print a licence".
2. **Bill and payment numbers use the UTC date.** `billLedger.nextDocumentNumber` uses
   `new Date().toISOString()`. In India (UTC+5:30), a bill made between midnight and 5:30 am gets
   **yesterday's date** in its number (e.g. SHOP-20261021-… on 22 Oct). Numbers stay unique.
3. **"Waiting to sync" only grows.** Cloud sync is not connected, so every bill/customer/product change is
   added to `sync_queue` and never removed; the header count rises forever and the table keeps growing.
4. **check-config.js still warns** "gst_enabled is true — GST is planned for version 2" (out of date).
5. **Window background colour** in main.js is the old blue-grey `#eef1f5` (brief flash before the page draws).
6. **Two bridge calls are never used**: `product:search`, `product:get`.
7. **Login lock is in memory**: restarting the app clears the 30 s lock.
8. **Backdated bills** get today's date in their bill number, not the chosen date.
9. **End-to-end test step 1 sometimes fails**: the 5-second video can finish before the test starts watching.
10. **`key.txt` in the project root holds the vendor reset key** in plain text.
11. **dev-data** holds old test bills made with free-typed products (from before "only listed products"); they
    still open and print, but those products are not in the product list.
12. **`docs/architecture.html`** is older than this file and doesn't show logins, backups, GST or farm details.
13. **The installer in `dist/` (built 18 Oct)** doesn't include the 21 Oct GST / discount / phone / Aadhaar changes.
