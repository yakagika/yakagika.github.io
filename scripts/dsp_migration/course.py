"""Check the hand-maintained course against the legacy chapters and layout."""

from pathlib import Path
import re
import subprocess

from markdown import MigrationError, pandoc_document, parse


COURSE_YAML = Path("courses/dsp.yaml")
MATERIALS = {
    "guide": (1, "@start", "本資料の読み方"),
    "scope": (2, "本科目で扱う範囲", "本科目で扱う範囲"),
    "capstone": (12, "@start", "総合実習"),
}
SESSIONS = (
    ("本資料の読み方", "guide"),
    ("分析設計: データ分析の進め方", "scope"),
    ("データの可視化", None),
    ("データの数値化", None),
    ("回帰分析", None),
    ("機械学習の枠組みと決定木", None),
    ("クラスタリングとパターン発見", None),
    ("ニューラルネットワークと Transformer", None),
    ("自然言語処理", None),
    ("画像認識", None),
    ("総合実習 1", "capstone"),
    ("総合実習 2", "capstone"),
    ("発表回", "capstone"),
)
# Fixed fifth session from the accepted regression course.
REGRESSION = (
    ("reg-intro", "required", ()),
    ("reg-ols", "advanced", ()),
    ("reg-simple", "required", (("exercise-reg-simple-1", "dsp5-1.py"),)),
    ("reg-multiple", "required", ()),
    ("reg-gpa", "required", ()),
    ("reg-prediction", "required", (("exercise-reg-prediction-1", None),)),
    ("reg-evaluate", "required", (("exercise-reg-evaluate-1", "dsp5-2.py"),)),
    ("reg-causal", "required", ()),
    ("reg-selection", "required", ()),
    ("reg-selection-gpa", "required", ()),
    ("reg-selection-lab", "advanced", ()),
    ("reg-glm", "advanced", ()),
    ("reg-logit", "advanced", ()),
)
SELECTION_INTRO = "授業では扱いませんが, 課題や研究で必要になったときに読んでください."


def material_path(name):
    return Path("courses", "dsp", f"{name}.md")


def exercise_file(block, name):
    segment = block.chapter.exercises[name]
    text = block.chapter.body[segment.start:segment.end]
    found = re.findall(r"提出ファイル(?:名:[ \t]*|は[ \t]+)`([^`\n]+)`", text)
    if len(found) > 1:
        raise MigrationError(f"{name} の提出ファイル名を一意に特定できません")
    return found[0] if found else None


def entry(block):
    return block.name, block.level, tuple((name, exercise_file(block, name)) for name in block.exercises)


def metadata_list(values):
    return {"t": "MetaList", "c": list(values)}


def metadata_text(text):
    return {"t": "MetaInlines", "c": parse(text)[0]["c"]}


def metadata_entry(description):
    name, level, exercises = description
    values = {"block": metadata_text(name), "level": metadata_text(level)}
    if exercises:
        def exercise_value(item):
            ident, filename = item
            fields = {"id": metadata_text(ident)}
            if filename:
                fields["file"] = metadata_text(filename)
            return {"t": "MetaMap", "c": fields}

        values["exercises"] = metadata_list(map(exercise_value, exercises))
    return {"t": "MetaMap", "c": values}


def verify_course(root, chapters, blocks, *, opened):
    path = root / COURSE_YAML
    try:
        metadata = pandoc_document("", "markdown-smart", metadata_file=path)["meta"]
    except subprocess.CalledProcessError as error:
        raise MigrationError(f"{COURSE_YAML} の構成を読めません ({error})") from error

    def get_list(key, size):
        value = metadata.get(key, {})
        if value.get("t") != "MetaList" or len(value["c"]) != size:
            raise MigrationError(f"{COURSE_YAML} の {key} は {size} 件のリストで指定してください")
        return value["c"]

    sessions = get_list("sessions", len(SESSIONS))
    selections = get_list("supplements", 1)

    def expect(actual, name, expected, label):
        if actual.get(name) != expected:
            raise MigrationError(f"{COURSE_YAML} の {label}の {name} が割り当てと一致しません")

    if opened is None:
        if metadata.get("open", {}).get("t") != "MetaBool":
            raise MigrationError(f"{COURSE_YAML} の open は true または false を指定してください")
    else:
        expect(metadata, "open", {"t": "MetaBool", "c": opened}, "講義")
    expect(metadata, "materials", None, "講義")
    expect(metadata, "title", metadata_text("データサイエンス実践"), "講義")
    card = (root / "pages/lectures.markdown").read_text(encoding="utf-8")
    descriptions = re.findall(
        r'<span class="series-title">データサイエンス実践</span>\s*<span class="series-desc">([^<]+)</span>',
        card,
    )
    if len(descriptions) != 1:
        raise MigrationError("講義一覧からデータサイエンス実践の説明を一意に特定できません")
    expect(metadata, "description", metadata_text(descriptions[0]), "講義")

    assignments = {str(n): [] for n in range(1, 14)}
    assignments["S"] = []
    for block in blocks.values():
        if block.session not in assignments or block.session == "5":
            raise MigrationError(f"{block.name} の回が講義の割り当てと一致しません")
        assignments[block.session].append(entry(block))
    assignments["5"] = REGRESSION
    required = sum(level == "required" for entries in assignments.values() for _, level, _ in entries)
    if required != 32:
        raise MigrationError(f"講義の必修ブロックが {required} 件です (期待値 32)")

    def check_section(value, title, material, key, label):
        if value.get("t") != "MetaMap":
            raise MigrationError(f"{COURSE_YAML} の {label} を読めません")
        data = value["c"]
        expect(data, "title", metadata_text(title), label)
        expect(data, "materials", metadata_list([metadata_text(material)]) if material else None, label)
        expect(data, "blocks", metadata_list(map(metadata_entry, assignments[key])), label)
        return data

    for number, ((title, material), session) in enumerate(zip(SESSIONS, sessions, strict=True), 1):
        label = f"第 {number} 回"
        data = check_section(session, title, material, str(number), label)
        if ("intro" in data) != (number == 5):
            raise MigrationError(f"{COURSE_YAML} の {label} の intro の有無が割り当てと一致しません")

    selected = check_section(selections[0], "選択資料", None, "S", "選択資料")
    expect(selected, "id", metadata_text("selected"), "選択資料")
    guide = chapters[1]
    _, heading = guide.boundary("選択の章")
    intro = guide.ast[heading + 1:heading + 2]
    expected = metadata_text(SELECTION_INTRO)
    if not intro or intro[0]["t"] != "Para" or intro[0]["c"][:len(expected["c"])] != expected["c"]:
        raise MigrationError("第 1 章の「選択の章」の冒頭文を特定できません")
    expect(selected, "intro", expected, "選択資料")
