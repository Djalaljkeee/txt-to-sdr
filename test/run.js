/*
 * Тесты конвертера. Запуск:  node test/run.js
 * Ядро (функции разбора/сборки) извлекается прямо из index.html — между
 * маркерами ==CORE-START== и ==CORE-END== — чтобы тестировался тот же код,
 * который выполняется в браузере.
 */
const fs = require("fs");
const path = require("path");
const assert = require("assert");
const vm = require("vm");

const html = fs.readFileSync(path.join(__dirname, "..", "index.html"), "utf8");
const core = html.split("/* ==CORE-START== */")[1].split("/* ==CORE-END== */")[0];
const ctx = { TextDecoder, TextEncoder, console };
vm.createContext(ctx);
vm.runInContext(core, ctx);

/* ---------- фикстуры ---------- */
const CRLF = "\r\n";
const SDR_HEADER = "00NMSDR33 V04-02.00                     213111";
const SDR_JOB    = "10NM                121111";
const SDR_SAMPLE = [
  SDR_HEADER,
  SDR_JOB,
  "08CO               138600.959       -22005.533      0                               ",
  "08CO               238597.409       -22003.196      0                               ",
  "08CO               338644           -21963.607      0                               ",
  "08CO               438653.77        -21956.455      0                               "
].join(CRLF) + CRLF;

const TXT_SAMPLE = [
  "G4,38500.4060,-21981.2990,236.1090,",
  "gg0.1,38589.4050,-22005.3930,222.6790,",
  "K10,38576.9320,-22008.7760,226.4890,",
  "M15,38548.2240,-21898.1460,243.2030,"
].join(CRLF) + CRLF;

const OPTS = {
  eol: "crlf", swap: false,
  sdrDecimals: 3, sdrStrip: true, sdrRenumber: false, sdrWriteCode: false, sdrSplit: 0,
  sdrHeader: SDR_HEADER, sdrJob: SDR_JOB,
  txtDecimals: 4, txtSep: ",", txtTail: true, txtWriteCode: false, txtSkipZeroZ: false
};
const opts = (over) => Object.assign({}, OPTS, over);

/* ---------- раннер ---------- */
let pass = 0, fail = 0;
function t(name, fn){
  try { fn(); console.log("  ok   " + name); pass++; }
  catch (e){ console.log("  FAIL " + name + "\n       " + e.message); fail++; }
}

/* ---------- определение формата ---------- */
t("определяет SDR", () => assert.equal(ctx.detectFormat(SDR_SAMPLE, "a.sdr"), "sdr"));
t("определяет TXT", () => assert.equal(ctx.detectFormat(TXT_SAMPLE, "a.txt"), "txt"));
t("пустой файл — по расширению", () => assert.equal(ctx.detectFormat("", "a.sdr"), "sdr"));

/* ---------- разбор SDR ---------- */
t("SDR: точки, заголовок, блоки", () => {
  const r = ctx.parseSdr(SDR_SAMPLE);
  assert.equal(r.points.length, 4);
  assert.equal(r.blocks, 1);
  assert.equal(r.header, SDR_HEADER);
  assert.equal(r.job, SDR_JOB);
  assert.deepEqual(r.warnings, []);
  assert.deepEqual(
    { name: r.points[0].name, x: r.points[0].x, y: r.points[0].y, z: r.points[0].z },
    { name: "1", x: 38600.959, y: -22005.533, z: 0 }
  );
});
t("SDR: битая строка даёт предупреждение", () => {
  const r = ctx.parseSdr(SDR_SAMPLE + "08CO               5---             xxx" + CRLF);
  assert.equal(r.points.length, 4);
  assert.equal(r.warnings.length, 1);
});
t("SDR: служебные записи считаются, но не ломают разбор", () => {
  const r = ctx.parseSdr(SDR_SAMPLE + "13NMкомментарий" + CRLF + "02TP  станция" + CRLF);
  assert.equal(r.points.length, 4);
  assert.ok(/служебных записей.*: 2/.test(r.warnings[0]));
});

/* ---------- разбор TXT ---------- */
t("TXT: разбор строк имя,X,Y,Z,", () => {
  const r = ctx.parseTxt(TXT_SAMPLE);
  assert.equal(r.points.length, 4);
  assert.equal(r.points[0].name, "G4");
  assert.equal(r.points[0].x, 38500.406);
  assert.equal(r.points[3].z, 243.203);
  assert.deepEqual(r.warnings, []);
});
t("TXT: пробелы, ; и строки без высоты", () => {
  const r = ctx.parseTxt(
    "P1 38600.959 -22005.533 12.5\nP2;38600.1;-22005.2;3\nP3,38600.2,-22005.3\n\nмусор в строке"
  );
  assert.equal(r.points.length, 3);
  assert.equal(r.points[0].name, "P1");
  assert.equal(r.points[2].z, 0);
  assert.equal(r.skipped, 1);
});
t("TXT: код точки после высоты", () => {
  const r = ctx.parseTxt("P1,38600.9,-22005.5,12.5,ЛЭП\n");
  assert.equal(r.points[0].code, "ЛЭП");
});

/* ---------- сборка SDR ---------- */
t("SDR: ширина записи ровно 84 символа", () => {
  const out = ctx.buildSdr(ctx.parseTxt(TXT_SAMPLE).points, opts());
  out.text.split(CRLF).filter(l => l.slice(0, 2) === "08")
     .forEach(l => assert.equal(l.length, 84));
});
t("SDR: раскладка колонок (имя вправо, числа влево)", () => {
  const out = ctx.buildSdr([{ name: "G4", x: 38500.406, y: -21981.299, z: 236.109, code: "" }], opts());
  const line = out.text.split(CRLF)[2];
  assert.equal(line.slice(0, 4), "08CO");
  assert.equal(line.slice(4, 20),  "              G4");
  assert.equal(line.slice(20, 36), "38500.406       ");
  assert.equal(line.slice(36, 52), "-21981.299      ");
  assert.equal(line.slice(52, 68), "236.109         ");
});
t("SDR: деление на блоки с повтором заголовка и нумерацией с 1", () => {
  const pts = [];
  for (let i = 0; i < 7; i++) pts.push({ name: "p" + i, x: i, y: -i, z: 0, code: "" });
  const out = ctx.buildSdr(pts, opts({ sdrSplit: 3, sdrRenumber: true }));
  const lines = out.text.split(CRLF).filter(Boolean);
  assert.equal(lines.filter(l => l.slice(0, 2) === "00").length, 3);
  assert.equal(lines.filter(l => l.slice(0, 2) === "08").length, 7);
  const first = lines.filter(l => l.slice(0, 2) === "08");
  assert.equal(first[0].slice(4, 20).trim(), "1");
  assert.equal(first[3].slice(4, 20).trim(), "1");
});
t("SDR: длинное имя обрезается с предупреждением", () => {
  const out = ctx.buildSdr([{ name: "A".repeat(20), x: 1, y: 2, z: 3, code: "" }], opts());
  assert.equal(out.warnings.length, 1);
  assert.equal(out.text.split(CRLF)[2].slice(4, 20), "A".repeat(16));
});
t("SDR: формат чисел (обрезка хвостовых нулей)", () => {
  assert.equal(ctx.formatNumber(38644, 3, true), "38644");
  assert.equal(ctx.formatNumber(38653.77, 3, true), "38653.77");
  assert.equal(ctx.formatNumber(-21999.866, 3, true), "-21999.866");
  assert.equal(ctx.formatNumber(-0.0001, 3, true), "0");
  assert.equal(ctx.formatNumber(38644, 3, false), "38644.000");
});

/* ---------- сборка TXT ---------- */
t("TXT: разделители и хвостовая запятая", () => {
  const pts = ctx.parseSdr(SDR_SAMPLE).points;
  assert.equal(ctx.buildTxt(pts, opts()).text.split(CRLF)[0], "1,38600.9590,-22005.5330,0.0000,");
  assert.equal(ctx.buildTxt(pts, opts({ txtTail: false })).text.split(CRLF)[0], "1,38600.9590,-22005.5330,0.0000");
  assert.equal(ctx.buildTxt(pts, opts({ txtSep: ";" })).text.split(CRLF)[0], "1;38600.9590;-22005.5330;0.0000;");
});
t("TXT: пропуск точек с нулевой высотой", () => {
  const pts = ctx.parseSdr(SDR_SAMPLE).points;
  const r = ctx.buildTxt(pts, opts({ txtSkipZeroZ: true }));
  assert.equal(r.text, "");
  assert.equal(r.warnings.length, 1);
});
t("TXT: перестановка X и Y", () => {
  const r = ctx.buildTxt([{ name: "P", x: 1, y: 2, z: 3, code: "" }], opts({ swap: true, txtDecimals: 1 }));
  assert.equal(r.text.trim(), "P,2.0,1.0,3.0,");
});

/* ---------- полные циклы ---------- */
t("SDR → TXT → SDR: побайтово совпадает с исходником", () => {
  const pts = ctx.parseSdr(SDR_SAMPLE).points;
  const txt = ctx.buildTxt(pts, opts({ txtDecimals: 3 })).text;
  const back = ctx.buildSdr(ctx.parseTxt(txt).points, opts()).text;
  assert.equal(back, SDR_SAMPLE);
});
t("TXT → SDR → TXT: побайтово совпадает с исходником", () => {
  const pts = ctx.parseTxt(TXT_SAMPLE).points;
  const sdr = ctx.buildSdr(pts, opts({ sdrDecimals: 4 })).text;
  const back = ctx.buildTxt(ctx.parseSdr(sdr).points, opts()).text;
  assert.equal(back, TXT_SAMPLE);
});
t("convert() выбирает направление сам", () => {
  const a = ctx.convert(SDR_SAMPLE, "x.sdr", opts());
  assert.equal(a.from + "->" + a.to, "sdr->txt");
  const b = ctx.convert(TXT_SAMPLE, "x.txt", opts());
  assert.equal(b.from + "->" + b.to, "txt->sdr");
  const c = ctx.convert(TXT_SAMPLE, "x.txt", opts({ force: "sdr" }));
  assert.equal(c.points.length, 0, "принудительный режим не выдумывает точки");
});

/* ---------- кодировки ---------- */
t("Windows-1251: кодирование и декодирование", () => {
  const s = "Точка №5 ГГ-1 ёЁ";
  assert.equal(ctx.cp1251Decode(ctx.cp1251Encode(s)), s);
  assert.equal(ctx.decodeBytes(ctx.cp1251Encode(s).buffer), s);
});
t("decodeBytes понимает UTF-8, BOM и ASCII", () => {
  const s = "Пикет 12";
  assert.equal(ctx.decodeBytes(new TextEncoder().encode(s).buffer), s);
  const bom = new Uint8Array([0xEF, 0xBB, 0xBF, ...new TextEncoder().encode(s)]);
  assert.equal(ctx.decodeBytes(bom.buffer), s);
  assert.equal(ctx.decodeBytes(new TextEncoder().encode("ABC,1,2,3,").buffer), "ABC,1,2,3,");
});
t("кириллица в именах доживает до SDR и обратно", () => {
  const src = "Пикет1,38600.9000,-22005.5000,12.3000,\r\n";
  const sdr = ctx.convert(src, "a.txt", opts({ sdrDecimals: 4 }));
  assert.equal(sdr.text.split(CRLF)[2].slice(4, 20).trim(), "Пикет1");
  const back = ctx.convert(sdr.text, "a.sdr", opts());
  assert.equal(back.text, src);
});

console.log("\n" + pass + " passed, " + fail + " failed");
process.exit(fail ? 1 : 0);
