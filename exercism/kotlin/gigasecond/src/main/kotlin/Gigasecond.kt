import java.time.Duration
import java.time.LocalDate
import java.time.LocalDateTime

class Gigasecond(current: LocalDateTime) {
    constructor(currentDate: LocalDate) : this(currentDate.atStartOfDay())

    val date: LocalDateTime = current.plus(Duration.ofSeconds(1_000_000_000))
}
