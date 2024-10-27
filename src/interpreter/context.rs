use std::collections::HashMap;

use anyhow::{anyhow, Result};

use super::Value;

#[derive(Debug)]
pub struct Context<'a> {
    environments: Vec<HashMap<&'a str, Value<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            environments: vec![HashMap::new()],
        }
    }

    pub fn get(&self, name: &'a str) -> Option<&Value<'a>> {
        for env in self.environments.iter().rev() {
            let value = env.get(name);
            if value.is_some() {
                return value;
            }
        }
        return None;
    }

    fn get_mut(&mut self, name: &'a str) -> Option<&mut Value<'a>> {
        for env in self.environments.iter_mut().rev() {
            let value = env.get_mut(name);
            if value.is_some() {
                return value;
            }
        }
        return None;
    }

    pub fn declare(&mut self, name: &'a str, value: Value<'a>) {
        let env = self.environments.last_mut().unwrap();
        env.insert(name, value);
    }

    pub fn assign(&mut self, name: &'a str, value: Value<'a>) -> Result<()> {
        let var = self
            .get_mut(name)
            .ok_or(anyhow!("Cannot find variable: {name}"))?;
        *var = value;
        Ok(())
    }

    pub fn enter_block(&mut self) {
        self.environments.push(HashMap::new());
    }

    pub fn exit_block(&mut self) {
        if self.environments.len() == 1 {
            return;
        }

        self.environments.pop();
    }
}
