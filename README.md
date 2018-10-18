# Exercise of Scala for Machine Learninig 2nd Edition

## 書籍

- [Scala for ML 2nd Edition](https://www.packtpub.com/big-data-and-business-intelligence/scala-machine-learning-second-edition)
- [Scala for ML の GitHub](https://github.com/PacktPublishing/Scala-for-Machine-Learning-Second-Edition)


# chap. 2

様々なモデルの形式.

- Parametric(パラメトリック): これは関数と方程式で構成されます (for example, y = sin(2t+w))
- Differential(微分): これは通常の微分方程式と偏微分方程式で構成されます (for example, dy = 2x.dx)
- Probabilistic(確率的): これは確率分布で構成されます (for example, p(x|c) = exp (k.logx – x)/x!)
- Graphical(グラフィカル): これは変数間の条件付独立を抽象化したグラフで構成されます (for example, p(x,y|c) = p(x|c).p(y|c))
- Directed graphs(有向性グラフ): これは時間的、空間的関係で構成されます (for example, scheduler)
- Numerical method(数値解析法): これは有限差分、有限要素またはニュートンラフソンなどの計算方法からなります
- Chemistry(化学): これは式および成分で構成されます (for example, H2O, Fe + C12 = FeC13)
- Taxonomy(分類): これは意味論的定義と概念の関係性で構成されます (for example, APG/Eudicots/Rosids/Huaceae/Malvales)
- Grammar and lexicon(文法とレキシコン): これは文書の構文表現で構成されます (for example, Scala programming language)
- Inference logic(推論ロジック): これはルールです (for example, IF (stock vol> 1.5 * average) AND rsi> 80 THEN …)

## モデルと設計

- モデリング：これは知っていることの記述です。モデルは仮定であり、正しいと証明された場合にアサーションになります（たとえば、米国の人口pが年1.2％増加、dp / dt = 1.012）。
- 設計：これは知らないものの表現を取り扱うことです。設計は、モデリングの探究段階と見なすことができます（例えば、米国人口の増加に寄与する特徴は何ですか？出生率？移民？経済条件？社会政策？）。

## feature-selection の 2ステップ

1. 新しい特徴のサブセットを探します
2. スコアリング機構を使って特徴のサブセットを評価します


## メトリクス

### 4つのファクタ

- True Positives(TPs): 正のデータで、正のラベルが付与されたもの
- True Negatives(TNs): 負のデータで、負のラベルが付与されたもの
- False Positives(FPs): 正のデータなのに、負のラベルが付与されたもの
- False Negatives(FNs): 負のデータなのに、正のラベルが付与されたもの

### 評価

- Accuracy: 正しく分類された割合(TP + TN)
- Precision: 正と予測したデータのうち，実際に正であるものの割合(TP/(TP + FP))
- Recall: 実際に正であるもののうち，正であると予測されたものの割合(TP/(TP + FN))
- F-measure: 調和平均(2 * Precision * Recall / (Precision + Recall))

## Area under the curves

- Area under the precision-recall curve (AuPRC)
- Area under the receiver operating characteristics (AuROC)
