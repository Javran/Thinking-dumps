#[derive(Debug)]
pub struct Duration(f64);

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Duration(s as f64)
    }
}

pub trait Planet {
    fn years_during(d: &Duration) -> f64 {
        unimplemented!(
            "convert a duration ({:?}) to the number of years on this planet for that duration",
            d,
        );
    }
}

pub struct Mercury;
pub struct Venus;
pub struct Earth;
pub struct Mars;
pub struct Jupiter;
pub struct Saturn;
pub struct Uranus;
pub struct Neptune;

macro_rules! impl_planet {
    ($planet:ident, $earth_years:expr) => {
        impl Planet for $planet {
            fn years_during(d: &Duration) -> f64 {
                d.0 / (31557600. * $earth_years)
            }
        }
    };
}

macro_rules! impl_planets {
    ($($planet:ident, $earth_years:expr),+) => {
        $(
            impl_planet!($planet, $earth_years);
        )+
    }
}

#[rustfmt::skip]
impl_planets!(
    Venus, 0.61519726,
    Mercury, 0.2408467,
    Earth, 1.,
    Mars, 1.8808158,
    Jupiter, 11.862615,
    Saturn, 29.447498,
    Uranus, 84.016846,
    Neptune, 164.79132
);
