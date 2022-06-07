module Pages.EditLog.Phrase exposing (phrase)


phrase :
    { pointBalance : String
    , pointBalanceIncludeChip : String
    , chip : String
    , balance : String
    , totalBalance : String
    , editLogConfigRate : String
    , editLogConfigChipRate : String
    , editLogConfigGameFee : String
    , editLogConfigHavePoint : String
    , editLogConfigReturnPoint : String
    , editLogConfigRankPointFirst : String
    , editLogConfigRankPointSecond : String
    , openEditLogConfigArea : String
    , closeEditLogConfigArea : String
    , openHowToUseArea : String
    , closeHowToUseArea : String
    , addRow : String
    , inputPoint : String
    , logo : String
    , exportToMiyabq : String
    , exportToMiyabqSending : String
    , exportToMiyabqSuccess : String
    , exportToMiyabqFailed : String
    , howToUse1 : String
    , howToUse2 : String
    , howToUse3 : String
    , howToUse4 : String
    }
phrase =
    { pointBalance = "合計"
    , pointBalanceIncludeChip = "合計\n(チップ込み)"
    , chip = "チップ\n(枚数)"
    , balance = "ポイント合計"
    , totalBalance = "ポイント合計\n(場代込み)"
    , editLogConfigRate = "倍率"
    , editLogConfigChipRate = "倍率(チップ)"
    , editLogConfigGameFee = "ゲーム代(一人当たり)"
    , editLogConfigHavePoint = "持ち点"
    , editLogConfigReturnPoint = "返し"
    , editLogConfigRankPointFirst = "ウマ(2, 3着)"
    , editLogConfigRankPointSecond = "ウマ(1, 4着)"
    , openEditLogConfigArea = "設定"
    , closeEditLogConfigArea = "設定を閉じる"
    , openHowToUseArea = "使い方"
    , closeHowToUseArea = "使い方を閉じる"
    , addRow = "行を追加する"
    , inputPoint = "🖋"
    , logo = "Jan Log"
    , exportToMiyabq = "Export To miyabq"
    , exportToMiyabqSending = "Sending..."
    , exportToMiyabqSuccess = "Success!"
    , exportToMiyabqFailed = "Request Failed.(時間をおいてもう一度試してください)"
    , howToUse1 = "1. 半荘が終了したら 🖋 をタップして素点を100で割った値を入力する"
    , howToUse2 = "※トビ賞も実際の点数を100で割った値を入力してください"
    , howToUse3 = "2. ゲームが終了したらチップ・場代などを入力する"
    , howToUse4 = "※レート・場代は「設定ボタン」から変更することができます"
    }
