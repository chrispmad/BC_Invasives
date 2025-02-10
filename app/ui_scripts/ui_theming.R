bcinv_theme = bs_theme(bootswatch = 'cerulean',
                       primary = "#168eb075",
                       secondary = "#48DAC6",
                       "font-size-base" = "0.8rem"
)

bcinv_theme = bs_add_rules(
  bcinv_theme,
  '.accordion-button:not(.collapsed){
    background: #168eb075;
  }

  .unhooked-btn {
    background: grey;
  }

  .hide_panel {
    display: hidden;
  }

  .rainbow-btn-container {
    width: 100%;
    height: 93%;
    display: flex;
    justify-content: center;
    align-items: center;
  }

  .rainbow-btn {
    border: none;
    outline: none;
    z-index: 0;
    background-color: white;
    border-radius: 12px;
  }

  .rainbow-btn::before {
    content: "";
    z-index: -1;
    position: absolute;
    background-color: white;
    width: 100%;
    height: 100%;
    left: 0;
    top: 0;
    border-radius: 10px;
  }

  .rainbow-btn::after {
    content: "";
    background: linear-gradient(
      45deg,
      #FF0000,
      #FF7300,
      #FFFB00,
      #48FF00,
      #00FFD5,
      #002BFF,
      #FF00C8,
      #FF0000
    );
    position: absolute;
    top: -2px;
    left: -2px;
    background-size: 600%;
    z-index: -2;
    width: calc(100% + 8px);
    height: calc(100% + 8px);
    filter: blur(8px);
    animation: glowing 20s linear infinite;
    transition: opacity .3s ease-in-out;
    border-radius: 10px;
    opacity: 1;
  }

  @keyframes glowing {
    0% {background-position: 0 0;}
    50% {background-position: 400% 0;}
    100% {background-position: 0 0;}
  }

  '
)
