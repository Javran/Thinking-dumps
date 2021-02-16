use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub struct Allergies(u32);

#[derive(Debug, PartialEq, Clone, Copy, EnumIter)]
pub enum Allergen {
    Eggs = 0x1,
    Peanuts = 0x2,
    Shellfish = 0x4,
    Strawberries = 0x8,
    Tomatoes = 0x10,
    Chocolate = 0x20,
    Pollen = 0x40,
    Cats = 0x80,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Allergies(score)
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        ((*allergen as u32) & self.0) != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        Allergen::iter()
            .filter(|x| self.is_allergic_to(x))
            .collect()
    }
}
