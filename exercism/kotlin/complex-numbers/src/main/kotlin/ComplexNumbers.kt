import kotlin.math.cos
import kotlin.math.exp
import kotlin.math.sin
import kotlin.math.sqrt

data class ComplexNumber(val real: Double = 0.0, val imag: Double = 0.0) {
    val abs = sqrt(real * real + imag * imag)

    operator fun plus(that: ComplexNumber) =
        ComplexNumber(real + that.real, imag + that.imag)

    operator fun minus(that: ComplexNumber) =
        ComplexNumber(real - that.real, imag - that.imag)

    operator fun times(that: ComplexNumber) =
        ComplexNumber(
            real * that.real - imag * that.imag,
            imag * that.real + real * that.imag
        )

    operator fun div(that: ComplexNumber) = (that.real * that.real + that.imag * that.imag).let { s ->
        ComplexNumber(
            (real * that.real + imag * that.imag) / s,
            (imag * that.real - real * that.imag) / s
        )
    }

    fun conjugate() = ComplexNumber(real, -imag)
}

fun exponential(c: ComplexNumber): ComplexNumber = exp(c.real).let { expA ->
    ComplexNumber(expA * cos(c.imag), expA * sin(c.imag))
}