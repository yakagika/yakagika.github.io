#!/usr/bin/env python3
"""合成 GPA の因果関係を比較する模式図を SVG と PNG で書き出す."""

from pathlib import Path
from subprocess import run

OUTPUT = (
    Path(__file__).resolve().parents[1]
    / "images/fields/regression/reg-causal/gpa-causal-graphs.svg"
)
WIDTH = 960
HEIGHT = 284
INK = "#22324a"
GRAY = "#66758b"
BLUE = "#225f99"

NODES = (
    ("アルバイト時間", 240, 100, 224),
    ("勉強時間", 100, 224, 148),
    ("GPA", 376, 224, 104),
)


def arrow(start: tuple[int, int], end: tuple[int, int], *, highlight: bool = False) -> str:
    color, width, marker = (BLUE, 4, "blue") if highlight else (GRAY, 2.5, "gray")
    x1, y1 = start
    x2, y2 = end
    return (
        f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{color}" '
        f'stroke-width="{width}" marker-end="url(#gpa-causal-{marker})"/>'
    )


def panel(left: int, title: str, study_work: tuple[tuple[int, int], tuple[int, int]]) -> str:
    elements = [
        f'<g transform="translate({left} 0)">',
        f'<text x="240" y="44" font-weight="600">{title}</text>',
        arrow((180, 224), (312, 224)),
        arrow((272, 130), (351, 192)),
        arrow(*study_work, highlight=True),
    ]
    for label, x, y, width in NODES:
        elements.extend(
            (
                f'<rect x="{x - width // 2}" y="{y - 26}" width="{width}" height="52" '
                f'rx="8" fill="#f7f8fa" stroke="{GRAY}" stroke-width="2"/>',
                f'<text x="{x}" y="{y}" dominant-baseline="central">{label}</text>',
            )
        )
    elements.append("</g>")
    return "\n    ".join(elements)


def main() -> None:
    study_port = (126, 192)
    work_port = (208, 130)
    svg = f"""<svg xmlns="http://www.w3.org/2000/svg" width="{WIDTH}" height="{HEIGHT}"
     viewBox="0 0 {WIDTH} {HEIGHT}" role="img" aria-labelledby="gpa-causal-title gpa-causal-desc">
  <title id="gpa-causal-title">アルバイト時間が媒介変数の場合と交絡因子の場合</title>
  <desc id="gpa-causal-desc">両方で勉強時間とアルバイト時間が GPA の原因になり, 媒介では勉強時間がアルバイト時間を変え, 交絡ではアルバイト時間が勉強時間を変える.</desc>
  <defs>
    <marker id="gpa-causal-gray" viewBox="0 0 12 12" markerWidth="12" markerHeight="12"
            refX="10" refY="6" orient="auto" markerUnits="userSpaceOnUse">
      <path d="M0 1 L12 6 L0 11 Z" fill="{GRAY}"/>
    </marker>
    <marker id="gpa-causal-blue" viewBox="0 0 16 16" markerWidth="16" markerHeight="16"
            refX="13" refY="8" orient="auto" markerUnits="userSpaceOnUse">
      <path d="M0 1 L16 8 L0 15 Z" fill="{BLUE}"/>
    </marker>
  </defs>
  <rect width="{WIDTH}" height="{HEIGHT}" fill="#fff"/>
  <line x1="480" y1="28" x2="480" y2="256" stroke="#c3cdd9" stroke-width="2"/>
  <g fill="{INK}" text-anchor="middle" font-size="26"
     font-family="Hiragino Sans, Noto Sans CJK JP, Arial, sans-serif">
    {panel(0, "(A) 媒介", (study_port, work_port))}
    {panel(480, "(B) 交絡", (work_port, study_port))}
  </g>
</svg>
"""
    OUTPUT.write_text(svg, encoding="utf-8")
    run(
        ["rsvg-convert", "-w", str(2 * WIDTH), "-o", str(OUTPUT.with_suffix(".png")), str(OUTPUT)],
        check=True,
    )


if __name__ == "__main__":
    main()
