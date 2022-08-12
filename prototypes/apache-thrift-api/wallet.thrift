namespace * Wallet

typedef string BlockHeaderHash
typedef i32 SlotNo
typedef i64 Height
typedef binary CBOR

struct BlockHeader {
  1: required SlotNo slotNo,
  2: required Height blockHeight,
  3: required BlockHeaderHash hash,
  4: optional BlockHeaderHash parentHeaderHash,
}

enum PostTransactionResult {
  MempoolFull,
  Accepted,
}

service Network {
  BlockHeader latestBlockHeader(),
  PostTransactionResult postTransaction(1: CBOR tx)
}
