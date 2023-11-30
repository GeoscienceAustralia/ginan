from pathlib import Path
from auto_generate_yaml import out_pea_yaml


def write_yaml(network_name):
    ginan_dir = Path('/var/ginan/')
    network_dir = ginan_dir / network_name
    out_pea_yaml(
        start_epoch = "",
        end_epoch = "",
        template_path = Path('templates/auto_template.yaml'),
        product_dir = network_dir, # / 'products',
        data_dir = network_dir, # / 'data'
        pea_out_dir = network_dir, # / 'outputs',
        config_out_dir = network_dir, # / 'generated_templates',
        relative_to_dir = None,
        trop_model="gpt2",
        trop_dir = None,
        enable_mongo = False,
        overrides=(),
    )