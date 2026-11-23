"""Draw the logistic function for the generalized linear models reference."""

from math import exp
from pathlib import Path


def main() -> None:
    width, height = 960, 584
    left, right = 120, 880
    top, bottom = 76, 456
    axis_y = 480
    eta_min, eta_max = -6, 6
    point_count = 201

    def x(eta: float) -> float:
        return left + (eta - eta_min) / (eta_max - eta_min) * (right - left)

    def y(probability: float) -> float:
        return bottom - probability * (bottom - top)

    points = []
    for index in range(point_count):
        eta = eta_min + (eta_max - eta_min) * index / (point_count - 1)
        probability = 1 / (1 + exp(-eta))
        points.append(f"{x(eta):.2f},{y(probability):.2f}")

    middle_x, middle_y = x(0), y(0.5)
    axis_color = "#1f3864"
    guide_color = "#73829a"
    curve_color = "#2f5597"
    x_ticks = []
    for eta in range(eta_min, eta_max + 1, 2):
        tick_x = x(eta)
        label = str(eta).replace("-", "−")
        x_ticks.append(
            f'    <path d="M{tick_x:.2f} {axis_y} v9" stroke="{axis_color}" />\n'
            f'    <text x="{tick_x:.2f}" y="526" text-anchor="middle">{label}</text>'
        )

    svg = f'''<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}"
     width="{width}" height="{height}" role="img" aria-labelledby="title desc">
  <title id="title">ロジスティック関数</title>
  <desc id="desc">横軸は線形予測子 η, 縦軸は確率 p.
  S 字の曲線は (0, 0.5) を通り, 左で p = 0 に, 右で p = 1 に近づく.</desc>
  <rect width="{width}" height="{height}" fill="white" />
  <path d="M{left} {top} H{right} M{left} {bottom} H{right}"
        fill="none" stroke="{guide_color}" stroke-width="2.5" stroke-dasharray="10 8" />
  <path d="M{left} {middle_y} H{middle_x} V{axis_y}"
        fill="none" stroke="{guide_color}" stroke-width="2" stroke-dasharray="4 7" />
  <path d="M{left} 56 V{axis_y} H{right}"
        fill="none" stroke="{axis_color}" stroke-width="3" />
  <path d="M{left - 9} {top} h9 M{left - 9} {middle_y} h9 M{left - 9} {bottom} h9"
        fill="none" stroke="{axis_color}" stroke-width="2" />
  <polyline points="{' '.join(points)}"
            fill="none" stroke="{curve_color}" stroke-width="5" stroke-linecap="round"
            stroke-linejoin="round" />
  <circle cx="{middle_x}" cy="{middle_y}" r="8" fill="{curve_color}"
          stroke="white" stroke-width="3" />
  <g font-family="Arial, sans-serif" font-size="30" fill="{axis_color}">
    <text x="96" y="{top + 10}" text-anchor="end">1</text>
    <text x="96" y="{middle_y + 10}" text-anchor="end">0.5</text>
    <text x="96" y="{bottom + 10}" text-anchor="end">0</text>
{chr(10).join(x_ticks)}
  </g>
  <g font-family="Times New Roman, serif" font-style="italic" font-size="40"
     fill="{axis_color}" text-anchor="middle">
    <text x="{left}" y="39">p</text>
    <text x="{middle_x}" y="568">η</text>
  </g>
</svg>
'''
    root = Path(__file__).resolve().parent.parent
    output = root / "images/fields/regression/reg-glm/logistic-function.svg"
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(svg, encoding="utf-8")


if __name__ == "__main__":
    main()
