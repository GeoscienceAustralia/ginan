from pathlib import Path
from auto_generate_yaml import out_pea_yaml


out_pea_yaml(
    start_epoch = "",
    end_epoch = "",
    template_path = Path('templates/auto_template.yaml'),
    product_dir = Path('/var/products'),
    data_dir = Path('/var/data'),
    pea_out_dir = Path('/var/outputs'),
    config_out_dir = Path('/var/generated_templates'),
    relative_to_dir = None,
    trop_model="gpt2",
    trop_dir = None,
    enable_mongo = False,
    overrides=(),
)