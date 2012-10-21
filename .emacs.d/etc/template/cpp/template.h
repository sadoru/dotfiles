/*---------------------------------------------------------------------------*/
/*
 * @(#)$Id:$
 * @(#)%file-without-ext% Class Header File
 *
 */
/*---------------------------------------------------------------------------*/
/**
 * @file  %file%
 * @brief %file-without-ext% Class Header File
 *
 * (★クラスの説明)
 *
 * @attention C++ 専用です C では使用出来ません
 */
/*---------------------------------------------------------------------------*/
#ifndef %include-guard%
#define %include-guard%

#ifndef __cplusplus
#	error ERROR: This file requires C++ compilation (use a .cpp suffix)
#endif

/*---------------------------------------------------------------------------*/
// Include Files

/*---------------------------------------------------------------------------*/
// Classの前方宣言

/*---------------------------------------------------------------------------*/
// 定数定義
/*---------------------------------------------------------------------------*/
// Classの定義
/**
 * %file-without-ext% Class
 *
 * (★クラスの説明)
 */

class %file-without-ext%
{
public:
    /*---------------------------------------------------------------------------*/
    // public Member関数定義 constructor、destructor

    /**
     * コンストラクタ
     *
     * @attention	なし
     * @param		なし
     * @return		なし
     */
    %file-without-ext%();

    /**
     * デストラクタ
     *
     * @attention	なし
     * @param		なし
     * @return		なし
     */
    virtual ~%file-without-ext%();

    /*---------------------------------------------------------------------------*/
    // public Member関数定義 公開I/F
    /**
     * (★method name)
     *
     * @attention	なし
     * @param		なし
     * @return		なし
     */
    virtual ~%file-without-ext%();

protected:
    /*---------------------------------------------------------------------------*/
    // protected Member関数定義

private:
    /*---------------------------------------------------------------------------*/
    // private Member変数定義

    /*---------------------------------------------------------------------------*/
    // private Member関数定義
    // copy禁止
    %file-without-ext%(const %file-without-ext% &x);
    // 代入禁止
    %file-without-ext%& operator=(const %file-without-ext% &x);
};
#endif // %include-guard%
/* EOF */
