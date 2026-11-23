---
title: "年齢識別のコード全体"
kind: code
open: true
requires:
  - img-age
---
## 年齢識別のコード全体 {#age-full-code}

[顔による年齢識別](block:img-age#age-classification)から[Grad-CAM と Guided Backpropagation](block:img-more#grad-cam)までのコードの全体は, 次のとおりです. 特徴マップ以降を試すたびに学習し直すのは大変なので, 2 回目以降は学習を飛ばし, 保存した重みを読み込むように分岐しています.

~~~ py
import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import random

# 次元削減用
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA

# CNN用
import torch
import torch.nn as nn
import torch.optim as optim
from torchvision import datasets, transforms, models
from torch.utils.data import DataLoader
from torchvision.models import ConvNeXt_Tiny_Weights


#学習した特徴量を抽出する
def extract_features(model, dataloader, device):
    model.eval()
    features = []
    labels_list = []

    with torch.no_grad():
        for inputs, labels in dataloader:
            inputs = inputs.to(device)
            labels = labels.to(device)
            outputs = model(inputs)
            features.append(outputs.cpu())
            labels_list.append(labels.cpu())

    features = torch.cat(features, dim=0)
    labels = torch.cat(labels_list, dim=0)

    return features, labels

# t-SNEによる次元圧縮と散布図の描画
def plot_tsne(train_features, train_labels, path):
    # train_featuresとtrain_labelsをnumpyに変換
    features_np = train_features.numpy()
    labels_np = train_labels.numpy()

    # t-SNEによる次元圧縮 (2次元)
    tsne = TSNE(n_components=2, random_state=42)
    features_2d = tsne.fit_transform(features_np)

    # 散布図の描画
    plt.figure(figsize=(10, 8))

    # 各ラベルに基づいて色分けしてプロット
    num_classes = len(np.unique(labels_np))  # クラス数を取得
    for label in np.unique(labels_np):
        indices = np.where(labels_np == label)
        plt.scatter(features_2d[indices, 0]
                   ,features_2d[indices, 1]
                   ,label=f'{(label + 1) * 10}s'
                   ,alpha=0.6)

    plt.title('t-SNE of Train Features')
    plt.xlabel('t-SNE Component 1')
    plt.ylabel('t-SNE Component 2')
    plt.legend(title="Age Group")
    plt.grid(True)
    plt.savefig(path)
    plt.close()

# PCAによる次元圧縮と散布図の描画
def plot_pca(train_features, train_labels, path):
    # train_featuresとtrain_labelsをnumpyに変換
    features_np = train_features.numpy()
    labels_np = train_labels.numpy()

    # PCAによる次元圧縮 (2次元)
    pca = PCA(n_components=2)
    features_2d = pca.fit_transform(features_np)

    # 散布図の描画
    plt.figure(figsize=(10, 8))

    # 各ラベルに基づいて色分けしてプロット
    num_classes = len(np.unique(labels_np))  # クラス数を取得
    for label in np.unique(labels_np):
        indices = np.where(labels_np == label)
        plt.scatter(features_2d[indices, 0]
                   ,features_2d[indices, 1]
                   ,label=f'{(label + 1) * 10}s'
                   ,alpha=0.6)

    plt.title('PCA of Train Features')
    plt.xlabel('PCA Component 1')
    plt.ylabel('PCA Component 2')
    plt.legend(title="Age Group")
    plt.grid(True)
    plt.savefig(path)
    plt.close()

# 乱数シードを設定
def set_seed(seed):
    torch.manual_seed(seed)
    np.random.seed(seed)              # Numpy用の乱数シードを設定
    random.seed(seed)                 # Pythonの標準乱数シードを設定

    # 再現性を完全に保証するために以下も設定（ただし、若干のパフォーマンス低下の可能性あり）
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

def main():
    # シードを設定する
    #(自分の研究でやる場合は以下の行は消しても問題ない.)
    set_seed(2024)
    # データのディレクトリ設定
    data_dir = 'data/sorted_images_split'
    # 結果の保存先のフォルダを作る
    os.makedirs('data/result', exist_ok=True)
    batch_size = 32
    num_epochs = 3
    num_classes = 6  # 10代, 20代, ..., 60代

    # デバイス設定
    #GPUが利用できる場合はGPUを使う,そうでない場合はCPUを計算に利用します.
    if torch.backends.mps.is_available():
        device = torch.device("mps") #Mac GPU
    elif torch.cuda.is_available():
        device = torch.device("cuda:0") #Win GPU
    else:
        device = torch.device("cpu") #CPU

    print(f'Using device: {device}')

    # モデル定義
    # ConvNextモデルの読み込みとカスタマイズ
    weights = ConvNeXt_Tiny_Weights.IMAGENET1K_V1  # 最新の重みを指定
    model = models.convnext_tiny(weights=weights)  # ConvNextの小さいモデルを使用
    #モデル分類層の最終層(第3層(0,1,2番目))の入力特徴量を取得
    num_ftrs = model.classifier[2].in_features
    #既に学習されたモデルではクラス数がことなるので ,入力特徴量の数(num_ftrs)はそのまま
    #出力をクラス数に変更
    model.classifier[2] = nn.Linear(num_ftrs, num_classes)
    model = model.to(device)
    # データ変換（前処理）
    data_transforms = {
        'train': transforms.Compose([
            transforms.Resize((224, 224)),     #画像のリサイズ
            transforms.RandomHorizontalFlip(), #画像をランダムに反転
            transforms.ColorJitter(brightness=0.2, contrast=0.2),  # 色調変化
            transforms.ToTensor(),             #テンソル(多次元配列)に変換
            transforms.Normalize(mean=[0.485, 0.456, 0.406]
                                ,std=[0.229, 0.224, 0.225]), #正規化(本来は値を変更する必要あり.
        ]),
        'val': transforms.Compose([
            transforms.Resize((224, 224)),
            transforms.ToTensor(),
            transforms.Normalize([0.485, 0.456, 0.406]
                                ,[0.229, 0.224, 0.225]),
        ]),
    }

    # データセットの読み込み
    image_datasets = {x: datasets.ImageFolder(os.path.join(data_dir, x),
                                              data_transforms[x])
                      for x in ['train', 'val']}
    dataloaders = {x: DataLoader(image_datasets[x]
                                ,batch_size=batch_size
                                ,shuffle=True
                                ,num_workers=4) #使用するCore数
                   for x in ['train', 'val']}

    # 毎回学習するの大変なので,学習済みの場合は結果を読み込む
    if os.path.exists('data/result/trained_model_weights.pth'):
        print("学習済みデータのロード中")
        model.load_state_dict(torch.load('data/result/trained_model_weights.pth', map_location=device))
    else:
        print("学習開始")
        # 損失関数とオプティマイザ
        criterion = nn.CrossEntropyLoss() #クロスエントロピー損失
        optimizer = optim.Adam(model.parameters() #Adam
                              ,lr=0.0001) #Learning rate (学習率)

        #SGDを利用する場合
        #optimizer = optim.SGD(model.parameters(), lr=0.001, momentum=0.9)

        #結果の記録用
        train_losses = []
        train_accuracies = []
        val_losses = []
        val_accuracies = []
        results = []

        # 学習ループ
        for epoch in range(num_epochs):
            print(f'Epoch {epoch+1}/{num_epochs}')
            print('-' * 10)

            for phase in ['train', 'val']:
                if phase == 'train':
                    model.train()
                else:
                    model.eval()

                running_loss = 0.0
                running_corrects = 0

                for inputs, labels in dataloaders[phase]:
                    inputs = inputs.to(device)
                    labels = labels.to(device)

                    optimizer.zero_grad()

                    with torch.set_grad_enabled(phase == 'train'):
                        outputs = model(inputs)
                        _, preds = torch.max(outputs, 1)
                        loss = criterion(outputs, labels)

                        if phase == 'train':
                            loss.backward()
                            optimizer.step()

                    running_loss += loss.item() * inputs.size(0)
                    running_corrects += torch.sum(preds == labels.data)
                    if phase == 'val':  # バリデーション時に予測と実際のラベルを保存
                        for i in range(len(labels)):
                            results.append({
                                'epoch': epoch + 1,
                                'pred': preds[i].item(),
                                'acctual': labels[i].item()
                            })

                epoch_loss = running_loss / len(image_datasets[phase])
                epoch_acc = running_corrects.float() / len(image_datasets[phase])

                print(f'{phase} Loss: {epoch_loss:.4f} Acc: {epoch_acc:.4f}')
                if phase == 'train':
                    train_losses.append(epoch_loss)
                    train_accuracies.append(epoch_acc.item())
                else:
                    val_losses.append(epoch_loss)
                    val_accuracies.append(epoch_acc.item())

        #------------------------------------------------------------------
        #結果の表示
        #------------------------------------------------------------------

        print('Training complete')
        print("Training Losses: ", train_losses)
        print("Training Accuracies: ", train_accuracies)
        print("Validation Losses: ", val_losses)
        print("Validation Accuracies: ", val_accuracies)

        # 結果を可視化してCSVファイルに保存
        loss_acc = pd.DataFrame({'train_losses':train_losses
                                ,'train_accuracies':train_accuracies
                                ,'val_losses':val_losses
                                ,'val_accuracies':val_accuracies})
        plt.title('Losses')
        plt.xlabel('Epoch')
        plt.ylabel('Losses')
        plt.plot(np.arange(num_epochs),loss_acc['train_losses'],c='r',label='train_losses')
        plt.plot(np.arange(num_epochs),loss_acc['val_losses'],c='b',label='val_losses')
        plt.grid(True)
        plt.legend()
        plt.savefig('data/result/convnext_loss.png')
        plt.close()

        plt.title('Accuracies')
        plt.xlabel('Epoch')
        plt.ylabel('Accuracies')
        plt.plot(np.arange(num_epochs),loss_acc['train_accuracies'],c='r',label='train_accuracies')
        plt.plot(np.arange(num_epochs),loss_acc['val_accuracies'],c='b',label='val_accuracies')
        plt.grid(True)
        plt.legend()
        plt.savefig('data/result/convnext_acc.png')
        plt.close()

        loss_acc.to_csv('data/result/convnext_loss_acc.csv'
                       ,encoding='utf_8_sig')

        results_df = pd.DataFrame(results)
        result_max_epochs = results_df[results_df['epoch'] == num_epochs]
        result_heatmap = pd.DataFrame(index=np.arange(6)
                                     ,columns=np.arange(6)
                                     ,data=0)

        #実際のラベルに対する予測された回数をカウント
        for i in result_max_epochs.index:
            p = result_max_epochs.at[i,'pred']
            a = result_max_epochs.at[i,'acctual']
            result_heatmap.at[p,a] +=1

        #列相対度数に変換
        for c in result_heatmap:
            result_heatmap[c] = result_heatmap[c] / result_heatmap[c].sum()

        #ヒートマップとして表現
        sns.heatmap(result_heatmap
                   ,annot=True)
        plt.ylabel('pred')
        plt.xlabel('acctual')
        plt.savefig('data/result/pred_acctual_heatmap.png')
        plt.close()
        results_df.to_csv('data/result/pred_acctual.csv'
                         ,encoding='utf_8_sig')

        # 特徴量を取得する
        train_features, train_labels = extract_features(model, dataloaders['train'], device)
        val_features, val_labels = extract_features(model, dataloaders['val'], device)


        #散布図の描画
        plot_tsne(train_features, train_labels,'data/result/convnext_tsne.png')
        plot_pca(train_features, train_labels,'data/result/convnext_pca.png')

        # (毎回学習するのは大変なので)特徴量を表示または保存しておく
        torch.save(model.state_dict(), 'data/result/trained_model_weights.pth')

    #------------------------------------------------------------------
    #特徴マップの描画
    #------------------------------------------------------------------
    # 中間層のフックを設定する層リストの設定例
    target_layers_indices = [0, 5, -1]  # お好みで変更可能

    # フックで取り出した特徴マップを保持する辞書
    intermediate_feature_maps = {}

    def get_intermediate_hook(name):
        def hook_fn(m, input, output):
            # output: [B, C, H, W]
            intermediate_feature_maps[name] = output.detach().cpu()
        return hook_fn

    # 対象層にフックを登録
    hooks = []
    for idx in target_layers_indices:
        layer_name = f"features_{idx}"
        h = model.features[idx].register_forward_hook(get_intermediate_hook(layer_name))
        hooks.append(h)

    # --- 最後の畳み込み層のフック ---
    feature_maps_last_conv = []
    def get_feature_map_hook_conv(self, input, output):
        feature_maps_last_conv.append(output.detach().cpu())

    hook_conv = model.features[-1].register_forward_hook(get_feature_map_hook_conv)


    # 抽出したいクラス (0:10s,1:20s,...,5:60s)
    target_classes = [0,1,2,3,4,5]
    # 各クラスから9枚ずつ取得するためのカウンタ
    images_per_class = 9
    extracted_counts = {cls: 0 for cls in target_classes}

    # 保存ディレクトリの用意
    base_dir = 'data/result/feature_maps'
    if not os.path.exists(base_dir):
        os.makedirs(base_dir)

    # クラスごとのフォルダを作成 (10s～60s)
    for cls in target_classes:
        class_label_str = f"{(cls+1)*10}s"
        class_dir = os.path.join(base_dir, class_label_str)
        if not os.path.exists(class_dir):
            os.makedirs(class_dir)
        # 中間層用のサブフォルダを用意
        for idx in target_layers_indices:
            layer_name = f"features_{idx}"
            layer_dir = os.path.join(class_dir, layer_name)
            if not os.path.exists(layer_dir):
                os.makedirs(layer_dir)

        # 最終Conv層用のフォルダ
        last_conv_dir = os.path.join(class_dir, 'last_conv')
        if not os.path.exists(last_conv_dir):
            os.makedirs(last_conv_dir)

    model.eval()
    val_iter = iter(dataloaders['val'])
    with torch.no_grad():
        # 全クラスが指定枚数分揃うまで繰り返す
        while not all(count == images_per_class for count in extracted_counts.values()):
            try:
                inputs, labels = next(val_iter)
            except StopIteration:
                # データがなくなったら終了
                break

            inputs = inputs.to(device)
            # フック用リストをクリア
            feature_maps_last_conv.clear()
            intermediate_feature_maps.clear()

            # 順伝播：フックで特徴マップ取得
            outputs = model(inputs)

            fm_batch = feature_maps_last_conv[0]   # [B, C, H, W] 最終Conv層出力

            for i in range(inputs.size(0)):
                cls = labels[i].item()
                if cls in target_classes and extracted_counts[cls] < images_per_class:

                    class_label_str = f"{(cls+1)*10}s"
                    class_dir = os.path.join(base_dir, class_label_str)

                    # 中間層特徴マップ可視化
                    for idx in target_layers_indices:
                        layer_name = f"features_{idx}"
                        fm_intermediate = intermediate_feature_maps[layer_name][i] # shape: [C, H, W]
                        num_channels_to_plot = min(8, fm_intermediate.shape[0])

                        fig, axes = plt.subplots(2, 4, figsize=(12, 6))
                        axes = axes.flatten()
                        for ch in range(num_channels_to_plot):
                            ax = axes[ch]
                            ax.imshow(fm_intermediate[ch].numpy(), cmap='viridis')
                            ax.axis('off')
                        plt.suptitle(f'Intermediate Layer {layer_name} Feature Maps: Class {(cls+1)*10}s (Image {extracted_counts[cls]+1})')

                        layer_dir = os.path.join(class_dir, layer_name)
                        out_path = os.path.join(layer_dir, f'feature_map_{extracted_counts[cls]+1}.png')
                        plt.savefig(out_path)
                        plt.close()

                    # 最終Conv層特徴マップ可視化
                    fm = fm_batch[i]  # shape: [C, H, W]
                    num_channels_to_plot = min(8, fm.shape[0])
                    fig, axes = plt.subplots(2, 4, figsize=(12, 6))
                    axes = axes.flatten()
                    for ch in range(num_channels_to_plot):
                        ax = axes[ch]
                        ax.imshow(fm[ch].numpy(), cmap='viridis')
                        ax.axis('off')
                    plt.suptitle(f'Last Conv Feature Maps: Class {(cls+1)*10}s (Image {extracted_counts[cls]+1})')
                    last_conv_dir = os.path.join(class_dir, 'last_conv')
                    out_path = os.path.join(last_conv_dir, f'feature_map_{extracted_counts[cls]+1}.png')
                    plt.savefig(out_path)
                    plt.close()

                    extracted_counts[cls] += 1

                    if all(count == images_per_class for count in extracted_counts.values()):
                        break

    # フックを削除
    hook_conv.remove()
    for h in hooks:
        h.remove()
    print('特徴マップ描画完了')

    #------------------------------------------------------------------
    # Grad-CAM, Guided Backprop, Guided Grad-CAM の実行 (10s, 60sに限定)
    #------------------------------------------------------------------
    import cv2

    # 対象クラスIDとクラス名
    target_cam_classes = {0:'10s', 5:'60s'}

    # valデータからサンプル画像を取得
    base_val_dir = os.path.join(data_dir, 'val')
    sample_images = {}
    for cls_id, cls_name in target_cam_classes.items():
        cls_dir = os.path.join(base_val_dir, cls_name)
        img_name = sorted(os.listdir(cls_dir))[0]  # ファイル名の順で最初の画像
        img_path = os.path.join(cls_dir, img_name)
        sample_images[cls_id] = img_path

    # Grad-CAM用フック設定
    target_layer = model.features[-1]
    features = None
    gradients = None

    def forward_hook(module, input, output):
        nonlocal features
        features = output

    def backward_hook(module, grad_input, grad_output):
        nonlocal gradients
        gradients = grad_output[0]

    forward_h = target_layer.register_forward_hook(forward_hook)
    backward_h = target_layer.register_full_backward_hook(backward_hook)

    # Guided Backprop用: ConvNeXtの活性化関数GELUの逆伝播を書き換える
    # ReLUのGuided Backpropと同じ規則で, 入力が正の位置にある正の勾配だけを通す
    gelu_inputs = {}  # moduleをキーにしてforwardの入力を保存

    def gelu_forward_hook(module, input, output):
        gelu_inputs[module] = input[0]

    def gelu_backward_hook(module, grad_input, grad_output):
        # grad_output[0]: GELUの出力についての勾配 (上の層から届いた勾配)
        # 返した値が, GELUの入力についての勾配として下の層へ伝わる
        positive_input = (gelu_inputs[module] > 0).float()  # 入力が正の位置だけ1
        positive_grad = torch.clamp(grad_output[0], min=0)  # 負の勾配を0にする
        return (positive_grad * positive_input,)

    # GELU層にフックを登録
    guided_hooks = []
    for m in model.modules():
        if isinstance(m, nn.GELU):
            fh = m.register_forward_hook(gelu_forward_hook)
            bh = m.register_full_backward_hook(gelu_backward_hook)
            guided_hooks.append(fh)
            guided_hooks.append(bh)

    def preprocess_image(img_path):
        img = Image.open(img_path).convert('RGB')
        img = img.resize((224, 224))
        img_tensor = data_transforms['val'](img).unsqueeze(0).to(device)
        return img, img_tensor

    def generate_gradcam():
        pooled_gradients = torch.mean(gradients, dim=[0,2,3])
        cam = torch.zeros(features.shape[2:], dtype=features.dtype, device=features.device)
        for i in range(features.shape[1]):
            cam += pooled_gradients[i] * features[0,i,:,:]
        cam = cam.cpu().data.numpy()
        cam = np.maximum(cam, 0)
        if np.max(cam) != 0:
            cam = cam / np.max(cam)
        return cam

    def normalize_gradient(grad):
        # 平均が0.5, 標準偏差が0.15になるように揃えて, 0〜1の範囲に収める
        # (最小値と最大値で0〜1に揃えると, 少数の極端な値に合わせて全体がほぼ灰色になる)
        grad = (grad - grad.mean()) / (grad.std() + 1e-8)
        return np.clip(grad * 0.15 + 0.5, 0, 1)

    def do_guided_backprop(model, img_tensor, target_class):
        # 勾配リセット
        model.zero_grad()
        img_tensor.grad = None
        # forward
        output = model(img_tensor)
        loss = output[0, target_class]
        model.zero_grad()
        loss.backward()
        # 入力画像についての勾配 (正規化する前の値) を返す
        guided_grad = img_tensor.grad.data[0].cpu().numpy().transpose(1,2,0)
        return guided_grad

    def apply_colormap_on_image(org_img, cam, alpha=0.5):
        H_org, W_org, _ = org_img.shape
        cam_resized = cv2.resize(cam, (W_org, H_org))

        heatmap = cv2.applyColorMap(np.uint8(255*cam_resized), cv2.COLORMAP_JET)
        heatmap = np.float32(heatmap)/255.0
        org_img = np.float32(org_img)/255.0

        cam_img = heatmap * alpha + org_img
        cam_img = cam_img / np.max(cam_img)
        return np.uint8(255*cam_img)

    def guided_gradcam(guided_grad, cam):
        # 正規化する前の勾配にGrad-CAMを掛けてから, 表示用に正規化する
        H, W, _ = guided_grad.shape
        cam_resized = cv2.resize(cam, (W, H))
        guided_gradcam = guided_grad * cam_resized[..., np.newaxis]
        return normalize_gradient(guided_gradcam)

    gradcam_dir = 'data/result/gradcam_guided'
    if not os.path.exists(gradcam_dir):
        os.makedirs(gradcam_dir)

    model.eval()
    from PIL import Image
    for cls_id, img_path in sample_images.items():
        # original画像読み込み
        org_img, img_tensor = preprocess_image(img_path)
        org_img_np = np.array(org_img)  # RGB, 224x224
        img_tensor.requires_grad = True

        # Grad-CAM
        model.zero_grad()
        output = model(img_tensor)
        target_score = output[0, cls_id]
        target_score.backward()
        cam = generate_gradcam()

        # Grad-CAM画像
        org_img_cv = org_img_np[:,:,::-1].copy()
        gradcam_img = apply_colormap_on_image(org_img_cv, cam)
        gradcam_img_rgb = gradcam_img[:,:,::-1]

        # Guided Backprop
        gb = do_guided_backprop(model, img_tensor, cls_id) # 正規化前の勾配
        g_gradcam = guided_gradcam(gb, cam) # 0-1 float

        original_uint8 = org_img_np
        gradcam_uint8 = gradcam_img_rgb
        gb_uint8 = (normalize_gradient(gb)*255).astype(np.uint8)
        g_gradcam_uint8 = (g_gradcam*255).astype(np.uint8)

        combined = np.hstack([original_uint8, gradcam_uint8, gb_uint8, g_gradcam_uint8])
        plt.imsave(os.path.join(gradcam_dir, f'class_{cls_id}_combined.png'), combined)

    forward_h.remove()
    backward_h.remove()
    for h in guided_hooks:
        h.remove()

    print("Grad-CAM, Guided Backprop, Guided Grad-CAM 完了")
#------------------------------------------------------------------
#スクリプトとして実行された場合(python convnext.py)で実行された場合に,
# if __name__ == '__main__': 以下のみが実行される.
# 並列処理(multiprocessing)を行う場合にこのようにしないと,
# 各処理ですべて同じコードが実行されるため,無限ループなどが起きる.
if __name__ == '__main__':
    main()
~~~

