class BankAccount() {
    private var _balance: Long? = 0
    var balance: Long
        get() {
            return _balance ?: throw IllegalStateException()
        }
        private set(x) {
            _balance ?: throw IllegalStateException()
            _balance = x
        }

    @Synchronized
    fun adjustBalance(amount: Long) {
        balance += amount
    }

    @Synchronized
    fun close() {
        _balance = null
    }
}
